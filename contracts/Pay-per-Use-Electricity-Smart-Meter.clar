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
