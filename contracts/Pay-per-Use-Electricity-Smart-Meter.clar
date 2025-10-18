(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-meter-not-found (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-meter-inactive (err u104))

(define-data-var rate-per-unit uint u100)
(define-data-var min-topup uint u1000)

(define-map meters
    principal
    {
        balance: uint,
        units-consumed: uint,
        last-reading: uint,
        active: bool,
        last-payment: uint,
    }
)

(define-map consumption-history
    {
        user: principal,
        timestamp: uint,
    }
    {
        units: uint,
        amount-paid: uint,
    }
)

(define-public (initialize-meter)
    (let ((caller tx-sender))
        (asserts! (is-none (map-get? meters caller)) (err u105))
        (ok (map-set meters caller {
            balance: u0,
            units-consumed: u0,
            last-reading: u0,
            active: true,
            last-payment: stacks-block-height,
        }))
    )
)

(define-public (top-up (amount uint))
    (let (
            (caller tx-sender)
            (meter-data (unwrap! (map-get? meters caller) err-meter-not-found))
        )
        (asserts! (>= amount (var-get min-topup)) err-invalid-amount)
        (try! (stx-transfer? amount caller contract-owner))
        (ok (map-set meters caller
            (merge meter-data {
                balance: (+ (get balance meter-data) amount),
                last-payment: stacks-block-height,
            })
        ))
    )
)

(define-public (consume-units (units uint))
    (let (
            (caller tx-sender)
            (meter-data (unwrap! (map-get? meters caller) err-meter-not-found))
            (cost (* units (var-get rate-per-unit)))
        )
        (asserts! (get active meter-data) err-meter-inactive)
        (asserts! (>= (get balance meter-data) cost) err-insufficient-balance)
        (map-set consumption-history {
            user: caller,
            timestamp: stacks-block-height,
        } {
            units: units,
            amount-paid: cost,
        })
        (ok (map-set meters caller
            (merge meter-data {
                balance: (- (get balance meter-data) cost),
                units-consumed: (+ (get units-consumed meter-data) units),
                last-reading: (+ (get last-reading meter-data) units),
            })
        ))
    )
)

(define-public (update-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set rate-per-unit new-rate))
    )
)

(define-public (update-min-topup (new-min uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set min-topup new-min))
    )
)

(define-public (toggle-meter-status (user principal))
    (let ((meter-data (unwrap! (map-get? meters user) err-meter-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set meters user
            (merge meter-data { active: (not (get active meter-data)) })
        ))
    )
)

(define-read-only (get-meter-info (user principal))
    (ok (unwrap! (map-get? meters user) err-meter-not-found))
)

(define-read-only (get-consumption-history
        (user principal)
        (timestamp uint)
    )
    (ok (map-get? consumption-history {
        user: user,
        timestamp: timestamp,
    }))
)

(define-read-only (get-current-rate)
    (ok (var-get rate-per-unit))
)
(define-constant emergency-credit-limit u5000)
(define-constant emergency-interest-rate u110)
(define-constant err-emergency-limit-exceeded (err u106))
(define-constant err-no-emergency-debt (err u107))

(define-data-var emergency-credit-enabled bool true)

(define-map emergency-accounts
    principal
    {
        credit-used: uint,
        credit-limit: uint,
        last-emergency-use: uint,
    }
)

(define-public (initialize-emergency-credit)
    (let ((caller tx-sender))
        (asserts! (is-some (map-get? meters caller)) err-meter-not-found)
        (ok (map-set emergency-accounts caller {
            credit-used: u0,
            credit-limit: emergency-credit-limit,
            last-emergency-use: u0,
        }))
    )
)

(define-public (use-emergency-credit (amount uint))
    (let (
            (caller tx-sender)
            (meter-data (unwrap! (map-get? meters caller) err-meter-not-found))
            (emergency-data (unwrap! (map-get? emergency-accounts caller) err-meter-not-found))
            (available-credit (- (get credit-limit emergency-data) (get credit-used emergency-data)))
        )
        (asserts! (var-get emergency-credit-enabled) err-meter-inactive)
        (asserts! (get active meter-data) err-meter-inactive)
        (asserts! (>= available-credit amount) err-emergency-limit-exceeded)
        (map-set emergency-accounts caller
            (merge emergency-data {
                credit-used: (+ (get credit-used emergency-data) amount),
                last-emergency-use: stacks-block-height,
            })
        )
        (ok (map-set meters caller
            (merge meter-data { balance: (+ (get balance meter-data) amount) })
        ))
    )
)

(define-public (repay-emergency-credit (amount uint))
    (let (
            (caller tx-sender)
            (emergency-data (unwrap! (map-get? emergency-accounts caller) err-meter-not-found))
            (debt-with-interest (* (get credit-used emergency-data) emergency-interest-rate))
            (actual-debt (/ debt-with-interest u100))
            (repay-amount (if (<= amount actual-debt)
                amount
                actual-debt
            ))
        )
        (asserts! (> (get credit-used emergency-data) u0) err-no-emergency-debt)
        (try! (stx-transfer? repay-amount caller contract-owner))
        (ok (map-set emergency-accounts caller
            (merge emergency-data { credit-used: (if (<= repay-amount actual-debt)
                (- (get credit-used emergency-data)
                    (/ (* repay-amount u100) emergency-interest-rate)
                )
                u0
            ) }
            )))
    )
)

(define-public (toggle-emergency-credit)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set emergency-credit-enabled
            (not (var-get emergency-credit-enabled))
        ))
    )
)

(define-read-only (get-emergency-info (user principal))
    (ok (map-get? emergency-accounts user))
)
(define-constant billing-cycle-monthly u4320)
(define-constant billing-cycle-weekly u1008)
(define-constant err-invalid-schedule (err u108))
(define-constant err-schedule-not-found (err u109))
(define-constant err-insufficient-scheduled-balance (err u110))

(define-map billing-schedules
    principal
    {
        auto-topup-amount: uint,
        billing-cycle: uint,
        next-billing: uint,
        active: bool,
        min-balance-trigger: uint,
    }
)

(define-map billing-history
    {
        user: principal,
        billing-id: uint,
    }
    {
        amount: uint,
        timestamp: uint,
        cycle-type: uint,
        success: bool,
    }
)

(define-data-var billing-counter uint u0)

(define-public (setup-auto-billing
        (topup-amount uint)
        (cycle uint)
        (min-trigger uint)
    )
    (let ((caller tx-sender))
        (asserts! (is-some (map-get? meters caller)) err-meter-not-found)
        (asserts! (>= topup-amount (var-get min-topup)) err-invalid-amount)
        (asserts!
            (or (is-eq cycle billing-cycle-weekly) (is-eq cycle billing-cycle-monthly))
            err-invalid-schedule
        )
        (ok (map-set billing-schedules caller {
            auto-topup-amount: topup-amount,
            billing-cycle: cycle,
            next-billing: (+ stacks-block-height cycle),
            active: true,
            min-balance-trigger: min-trigger,
        }))
    )
)

(define-public (process-auto-billing (user principal))
    (let (
            (meter-data (unwrap! (map-get? meters user) err-meter-not-found))
            (schedule-data (unwrap! (map-get? billing-schedules user) err-schedule-not-found))
            (current-height stacks-block-height)
            (billing-id (var-get billing-counter))
        )
        (asserts! (get active schedule-data) err-meter-inactive)
        (asserts!
            (or
                (>= current-height (get next-billing schedule-data))
                (<= (get balance meter-data)
                    (get min-balance-trigger schedule-data)
                )
            )
            err-invalid-schedule
        )
        (var-set billing-counter (+ billing-id u1))
        (match (stx-transfer? (get auto-topup-amount schedule-data) user contract-owner)
            success (begin
                (map-set meters user
                    (merge meter-data {
                        balance: (+ (get balance meter-data)
                            (get auto-topup-amount schedule-data)
                        ),
                        last-payment: current-height,
                    })
                )
                (map-set billing-schedules user
                    (merge schedule-data { next-billing: (+ current-height (get billing-cycle schedule-data)) })
                )
                (map-set billing-history {
                    user: user,
                    billing-id: billing-id,
                } {
                    amount: (get auto-topup-amount schedule-data),
                    timestamp: current-height,
                    cycle-type: (get billing-cycle schedule-data),
                    success: true,
                })
                (ok billing-id)
            )
            error (begin
                (map-set billing-history {
                    user: user,
                    billing-id: billing-id,
                } {
                    amount: (get auto-topup-amount schedule-data),
                    timestamp: current-height,
                    cycle-type: (get billing-cycle schedule-data),
                    success: false,
                })
                err-insufficient-scheduled-balance
            )
        )
    )
)

(define-public (update-billing-schedule
        (topup-amount uint)
        (cycle uint)
        (min-trigger uint)
    )
    (let (
            (caller tx-sender)
            (schedule-data (unwrap! (map-get? billing-schedules caller) err-schedule-not-found))
        )
        (asserts! (>= topup-amount (var-get min-topup)) err-invalid-amount)
        (asserts!
            (or (is-eq cycle billing-cycle-weekly) (is-eq cycle billing-cycle-monthly))
            err-invalid-schedule
        )
        (ok (map-set billing-schedules caller
            (merge schedule-data {
                auto-topup-amount: topup-amount,
                billing-cycle: cycle,
                min-balance-trigger: min-trigger,
                next-billing: (+ stacks-block-height cycle),
            })
        ))
    )
)

(define-public (toggle-auto-billing)
    (let (
            (caller tx-sender)
            (schedule-data (unwrap! (map-get? billing-schedules caller) err-schedule-not-found))
        )
        (ok (map-set billing-schedules caller
            (merge schedule-data { active: (not (get active schedule-data)) })
        ))
    )
)

(define-read-only (get-billing-schedule (user principal))
    (ok (map-get? billing-schedules user))
)

(define-read-only (get-billing-history
        (user principal)
        (billing-id uint)
    )
    (ok (map-get? billing-history {
        user: user,
        billing-id: billing-id,
    }))
)

(define-read-only (check-billing-due (user principal))
    (match (map-get? billing-schedules user)
        schedule-data (ok (>= stacks-block-height (get next-billing schedule-data)))
        (ok false)
    )
)

(define-constant time-peak u1)
(define-constant time-off-peak u2)
(define-constant time-weekend u3)
(define-constant err-invalid-time-period (err u111))

(define-data-var peak-rate uint u150)
(define-data-var off-peak-rate uint u80)
(define-data-var weekend-rate uint u90)
(define-data-var time-based-pricing-enabled bool false)

(define-map time-periods
    uint
    {
        start-hour: uint,
        end-hour: uint,
        days: uint,
        rate-multiplier: uint,
    }
)

(define-map consumption-by-period
    {
        user: principal,
        period: uint,
        date: uint,
    }
    {
        units: uint,
        amount-paid: uint,
        rate-used: uint,
    }
)

(define-private (get-time-period (height uint))
    (let (
            (hour (mod (/ height u10) u24))
            (day-of-week (mod (/ height u240) u7))
        )
        (if (or (is-eq day-of-week u6) (is-eq day-of-week u0))
            time-weekend
            (if (and (>= hour u17) (<= hour u21))
                time-peak
                time-off-peak
            )
        )
    )
)

(define-private (get-rate-for-period (period uint))
    (if (var-get time-based-pricing-enabled)
        (if (is-eq period time-peak)
            (var-get peak-rate)
            (if (is-eq period time-off-peak)
                (var-get off-peak-rate)
                (if (is-eq period time-weekend)
                    (var-get weekend-rate)
                    (var-get rate-per-unit)
                )
            )
        )
        (var-get rate-per-unit)
    )
)

(define-public (consume-units-time-based (units uint))
    (let (
            (caller tx-sender)
            (meter-data (unwrap! (map-get? meters caller) err-meter-not-found))
            (current-period (get-time-period stacks-block-height))
            (current-rate (get-rate-for-period current-period))
            (cost (* units current-rate))
            (date (/ stacks-block-height u1440))
        )
        (asserts! (get active meter-data) err-meter-inactive)
        (asserts! (>= (get balance meter-data) cost) err-insufficient-balance)
        (map-set consumption-history {
            user: caller,
            timestamp: stacks-block-height,
        } {
            units: units,
            amount-paid: cost,
        })
        (map-set consumption-by-period {
            user: caller,
            period: current-period,
            date: date,
        } {
            units: units,
            amount-paid: cost,
            rate-used: current-rate,
        })
        (ok (map-set meters caller
            (merge meter-data {
                balance: (- (get balance meter-data) cost),
                units-consumed: (+ (get units-consumed meter-data) units),
                last-reading: (+ (get last-reading meter-data) units),
            })
        ))
    )
)

(define-public (configure-time-based-pricing
        (peak-rate-new uint)
        (off-peak-rate-new uint)
        (weekend-rate-new uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set peak-rate peak-rate-new)
        (var-set off-peak-rate off-peak-rate-new)
        (var-set weekend-rate weekend-rate-new)
        (ok true)
    )
)

(define-public (toggle-time-based-pricing)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set time-based-pricing-enabled
            (not (var-get time-based-pricing-enabled))
        ))
    )
)

(define-read-only (get-time-based-rates)
    (ok {
        peak-rate: (var-get peak-rate),
        off-peak-rate: (var-get off-peak-rate),
        weekend-rate: (var-get weekend-rate),
        enabled: (var-get time-based-pricing-enabled),
    })
)

(define-read-only (get-current-time-period)
    (ok (get-time-period stacks-block-height))
)

(define-read-only (get-period-consumption
        (user principal)
        (period uint)
        (date uint)
    )
    (ok (map-get? consumption-by-period {
        user: user,
        period: period,
        date: date,
    }))
)

(define-constant points-per-topup u10)
(define-constant points-per-off-peak-unit u2)
(define-constant points-per-conservation u50)
(define-constant points-redemption-rate u100)
(define-constant loyalty-level-bronze u500)
(define-constant loyalty-level-silver u1500)
(define-constant loyalty-level-gold u3000)
(define-constant conservation-threshold-percent u85)
(define-constant err-insufficient-points (err u112))
(define-constant err-loyalty-not-initialized (err u113))

(define-data-var loyalty-program-enabled bool true)

(define-map loyalty-accounts
    principal
    {
        total-points: uint,
        available-points: uint,
        loyalty-level: uint,
        last-reward-date: uint,
        topup-streak: uint,
        total-redeemed: uint,
        baseline-consumption: uint,
    }
)

(define-map loyalty-history
    {
        user: principal,
        transaction-id: uint,
    }
    {
        points-earned: uint,
        action-type: uint,
        timestamp: uint,
        description: (string-ascii 64),
    }
)

(define-map redemption-history
    {
        user: principal,
        redemption-id: uint,
    }
    {
        points-used: uint,
        credit-amount: uint,
        timestamp: uint,
    }
)

(define-data-var loyalty-transaction-counter uint u0)
(define-data-var redemption-counter uint u0)

(define-private (get-loyalty-level (points uint))
    (if (>= points loyalty-level-gold)
        u3
        (if (>= points loyalty-level-silver)
            u2
            (if (>= points loyalty-level-bronze)
                u1
                u0
            )
        )
    )
)

(define-private (award-points
        (user principal)
        (points uint)
        (action-type uint)
        (description (string-ascii 64))
    )
    (let (
            (loyalty-data (unwrap! (map-get? loyalty-accounts user) (err u0)))
            (transaction-id (var-get loyalty-transaction-counter))
            (new-total (+ (get total-points loyalty-data) points))
            (new-available (+ (get available-points loyalty-data) points))
            (new-level (get-loyalty-level new-total))
        )
        (var-set loyalty-transaction-counter (+ transaction-id u1))
        (map-set loyalty-accounts user
            (merge loyalty-data {
                total-points: new-total,
                available-points: new-available,
                loyalty-level: new-level,
                last-reward-date: stacks-block-height,
            })
        )
        (map-set loyalty-history {
            user: user,
            transaction-id: transaction-id,
        } {
            points-earned: points,
            action-type: action-type,
            timestamp: stacks-block-height,
            description: description,
        })
        (ok points)
    )
)

(define-public (initialize-loyalty-account)
    (let ((caller tx-sender))
        (asserts! (is-some (map-get? meters caller)) err-meter-not-found)
        (asserts! (is-none (map-get? loyalty-accounts caller)) (err u105))
        (ok (map-set loyalty-accounts caller {
            total-points: u0,
            available-points: u0,
            loyalty-level: u0,
            last-reward-date: stacks-block-height,
            topup-streak: u0,
            total-redeemed: u0,
            baseline-consumption: u1000,
        }))
    )
)

(define-public (top-up-with-loyalty (amount uint))
    (let (
            (caller tx-sender)
            (meter-data (unwrap! (map-get? meters caller) err-meter-not-found))
        )
        (asserts! (>= amount (var-get min-topup)) err-invalid-amount)
        (try! (stx-transfer? amount caller contract-owner))
        (map-set meters caller
            (merge meter-data {
                balance: (+ (get balance meter-data) amount),
                last-payment: stacks-block-height,
            })
        )
        (if (and
                (var-get loyalty-program-enabled)
                (is-some (map-get? loyalty-accounts caller))
            )
            (begin
                (try! (award-points caller points-per-topup u1 "Topup Reward"))
                (ok true)
            )
            (ok true)
        )
    )
)

(define-public (consume-units-with-loyalty (units uint))
    (let (
            (caller tx-sender)
            (meter-data (unwrap! (map-get? meters caller) err-meter-not-found))
            (current-period (get-time-period stacks-block-height))
            (current-rate (get-rate-for-period current-period))
            (cost (* units current-rate))
            (date (/ stacks-block-height u1440))
        )
        (asserts! (get active meter-data) err-meter-inactive)
        (asserts! (>= (get balance meter-data) cost) err-insufficient-balance)
        (map-set consumption-history {
            user: caller,
            timestamp: stacks-block-height,
        } {
            units: units,
            amount-paid: cost,
        })
        (map-set consumption-by-period {
            user: caller,
            period: current-period,
            date: date,
        } {
            units: units,
            amount-paid: cost,
            rate-used: current-rate,
        })
        (map-set meters caller
            (merge meter-data {
                balance: (- (get balance meter-data) cost),
                units-consumed: (+ (get units-consumed meter-data) units),
                last-reading: (+ (get last-reading meter-data) units),
            })
        )
        (if (and
                (var-get loyalty-program-enabled)
                (is-some (map-get? loyalty-accounts caller))
                (is-eq current-period time-off-peak)
            )
            (begin
                (try! (award-points caller (* units points-per-off-peak-unit) u2
                    "Off-Peak Usage"
                ))
                (ok true)
            )
            (ok true)
        )
    )
)

(define-public (redeem-points-for-credit (points uint))
    (let (
            (caller tx-sender)
            (loyalty-data (unwrap! (map-get? loyalty-accounts caller)
                err-loyalty-not-initialized
            ))
            (meter-data (unwrap! (map-get? meters caller) err-meter-not-found))
            (credit-amount (/ points points-redemption-rate))
            (redemption-id (var-get redemption-counter))
        )
        (asserts! (>= (get available-points loyalty-data) points)
            err-insufficient-points
        )
        (asserts! (> credit-amount u0) err-invalid-amount)
        (var-set redemption-counter (+ redemption-id u1))
        (map-set loyalty-accounts caller
            (merge loyalty-data {
                available-points: (- (get available-points loyalty-data) points),
                total-redeemed: (+ (get total-redeemed loyalty-data) points),
            })
        )
        (map-set meters caller
            (merge meter-data { balance: (+ (get balance meter-data) credit-amount) })
        )
        (map-set redemption-history {
            user: caller,
            redemption-id: redemption-id,
        } {
            points-used: points,
            credit-amount: credit-amount,
            timestamp: stacks-block-height,
        })
        (ok credit-amount)
    )
)

(define-public (check-conservation-reward)
    (let (
            (caller tx-sender)
            (meter-data (unwrap! (map-get? meters caller) err-meter-not-found))
            (loyalty-data (unwrap! (map-get? loyalty-accounts caller)
                err-loyalty-not-initialized
            ))
            (current-consumption (get units-consumed meter-data))
            (baseline (get baseline-consumption loyalty-data))
            (conservation-percentage (/ (* current-consumption u100) baseline))
        )
        (if (and
                (var-get loyalty-program-enabled)
                (<= conservation-percentage conservation-threshold-percent)
                (> baseline u0)
            )
            (begin
                (try! (award-points caller points-per-conservation u3
                    "Energy Conservation"
                ))
                (ok true)
            )
            (ok true)
        )
    )
)

(define-public (toggle-loyalty-program)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (var-set loyalty-program-enabled (not (var-get loyalty-program-enabled))))
    )
)

(define-read-only (get-loyalty-info (user principal))
    (ok (map-get? loyalty-accounts user))
)

(define-read-only (get-loyalty-history
        (user principal)
        (transaction-id uint)
    )
    (ok (map-get? loyalty-history {
        user: user,
        transaction-id: transaction-id,
    }))
)

(define-read-only (get-redemption-history
        (user principal)
        (redemption-id uint)
    )
    (ok (map-get? redemption-history {
        user: user,
        redemption-id: redemption-id,
    }))
)

(define-read-only (get-loyalty-program-status)
    (ok (var-get loyalty-program-enabled))
)

(define-read-only (calculate-redemption-value (points uint))
    (ok (/ points points-redemption-rate))
)

(define-constant voucher-validity-blocks u52560)
(define-constant err-voucher-not-found (err u114))
(define-constant err-voucher-already-redeemed (err u115))
(define-constant err-voucher-expired (err u116))
(define-constant err-invalid-voucher-code (err u117))

(define-data-var voucher-counter uint u0)

(define-map vouchers
    uint
    {
        voucher-code: (string-ascii 32),
        credit-amount: uint,
        created-by: principal,
        created-at: uint,
        expires-at: uint,
        redeemed: bool,
        redeemed-by: (optional principal),
        redeemed-at: (optional uint),
    }
)

(define-map voucher-code-to-id
    (string-ascii 32)
    uint
)

(define-map user-voucher-redemptions
    {
        user: principal,
        redemption-index: uint,
    }
    {
        voucher-id: uint,
        credit-amount: uint,
        redeemed-at: uint,
    }
)

(define-data-var user-redemption-counter uint u0)

(define-public (generate-voucher
        (voucher-code (string-ascii 32))
        (credit-amount uint)
    )
    (let (
            (voucher-id (var-get voucher-counter))
            (current-height stacks-block-height)
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> credit-amount u0) err-invalid-amount)
        (asserts! (is-none (map-get? voucher-code-to-id voucher-code))
            err-invalid-voucher-code
        )
        (var-set voucher-counter (+ voucher-id u1))
        (map-set vouchers voucher-id {
            voucher-code: voucher-code,
            credit-amount: credit-amount,
            created-by: tx-sender,
            created-at: current-height,
            expires-at: (+ current-height voucher-validity-blocks),
            redeemed: false,
            redeemed-by: none,
            redeemed-at: none,
        })
        (map-set voucher-code-to-id voucher-code voucher-id)
        (ok voucher-id)
    )
)

(define-public (redeem-voucher (voucher-code (string-ascii 32)))
    (let (
            (caller tx-sender)
            (meter-data (unwrap! (map-get? meters caller) err-meter-not-found))
            (voucher-id (unwrap! (map-get? voucher-code-to-id voucher-code)
                err-voucher-not-found
            ))
            (voucher-data (unwrap! (map-get? vouchers voucher-id) err-voucher-not-found))
            (current-height stacks-block-height)
            (redemption-index (var-get user-redemption-counter))
        )
        (asserts! (not (get redeemed voucher-data)) err-voucher-already-redeemed)
        (asserts! (<= current-height (get expires-at voucher-data))
            err-voucher-expired
        )
        (var-set user-redemption-counter (+ redemption-index u1))
        (map-set vouchers voucher-id
            (merge voucher-data {
                redeemed: true,
                redeemed-by: (some caller),
                redeemed-at: (some current-height),
            })
        )
        (map-set meters caller
            (merge meter-data { balance: (+ (get balance meter-data) (get credit-amount voucher-data)) })
        )
        (map-set user-voucher-redemptions {
            user: caller,
            redemption-index: redemption-index,
        } {
            voucher-id: voucher-id,
            credit-amount: (get credit-amount voucher-data),
            redeemed-at: current-height,
        })
        (ok (get credit-amount voucher-data))
    )
)

(define-public (batch-generate-vouchers
        (voucher-codes (list 10 (string-ascii 32)))
        (credit-amounts (list 10 uint))
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map generate-voucher-internal voucher-codes credit-amounts))
    )
)

(define-private (generate-voucher-internal
        (voucher-code (string-ascii 32))
        (credit-amount uint)
    )
    (let (
            (voucher-id (var-get voucher-counter))
            (current-height stacks-block-height)
        )
        (if (and
                (> credit-amount u0)
                (is-none (map-get? voucher-code-to-id voucher-code))
            )
            (begin
                (var-set voucher-counter (+ voucher-id u1))
                (map-set vouchers voucher-id {
                    voucher-code: voucher-code,
                    credit-amount: credit-amount,
                    created-by: contract-owner,
                    created-at: current-height,
                    expires-at: (+ current-height voucher-validity-blocks),
                    redeemed: false,
                    redeemed-by: none,
                    redeemed-at: none,
                })
                (map-set voucher-code-to-id voucher-code voucher-id)
                voucher-id
            )
            u0
        )
    )
)

(define-read-only (get-voucher-by-code (voucher-code (string-ascii 32)))
    (match (map-get? voucher-code-to-id voucher-code)
        voucher-id (ok (map-get? vouchers voucher-id))
        err-voucher-not-found
    )
)

(define-read-only (get-voucher-by-id (voucher-id uint))
    (ok (map-get? vouchers voucher-id))
)

(define-read-only (check-voucher-validity (voucher-code (string-ascii 32)))
    (match (map-get? voucher-code-to-id voucher-code)
        voucher-id (match (map-get? vouchers voucher-id)
            voucher-data (ok {
                valid: (and
                    (not (get redeemed voucher-data))
                    (<= stacks-block-height (get expires-at voucher-data))
                ),
                credit-amount: (get credit-amount voucher-data),
                expires-at: (get expires-at voucher-data),
            })
            err-voucher-not-found
        )
        err-voucher-not-found
    )
)

(define-read-only (get-user-voucher-redemption
        (user principal)
        (redemption-index uint)
    )
    (ok (map-get? user-voucher-redemptions {
        user: user,
        redemption-index: redemption-index,
    }))
)

(define-read-only (get-total-vouchers-generated)
    (ok (var-get voucher-counter))
)

(define-read-only (get-total-redemptions)
    (ok (var-get user-redemption-counter))
)
