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
