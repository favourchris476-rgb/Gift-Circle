(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-joined (err u102))
(define-constant err-insufficient-amount (err u103))
(define-constant err-circle-not-active (err u104))
(define-constant err-circle-not-ready (err u105))
(define-constant err-already-matched (err u106))
(define-constant err-not-participant (err u107))
(define-constant err-already-claimed (err u108))
(define-constant err-invalid-minimum (err u109))

(define-constant minimum-gift-amount u1000000)
;; Max participants per circle to support iterative matching
(define-constant max-participants u100)
(define-constant participant-indices (list
    u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19
    u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37
    u38 u39 u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55
    u56 u57 u58 u59 u60 u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73
    u74 u75 u76 u77 u78 u79 u80 u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91
    u92 u93 u94 u95 u96 u97 u98 u99
))

(define-data-var circle-id uint u0)
(define-data-var current-circle-active bool false)
(define-data-var current-circle-minimum uint minimum-gift-amount)
(define-data-var matching-nonce uint u0)

(define-map circles
    uint
    {
        active: bool,
        minimum-amount: uint,
        participant-count: uint,
        matched: bool,
        total-deposited: uint,
    }
)

(define-map participants
    {
        circle-id: uint,
        participant: principal,
    }
    {
        deposited: uint,
        recipient: (optional principal),
        giver: (optional principal),
        claimed: bool,
    }
)

(define-map circle-participants
    {
        circle-id: uint,
        index: uint,
    }
    principal
)

(define-map participant-index
    {
        circle-id: uint,
        participant: principal,
    }
    uint
)

;; --- Read-Only Functions ---

(define-read-only (get-current-circle-id)
    (var-get circle-id)
)

(define-read-only (get-circle-info (cid uint))
    (map-get? circles cid)
)

(define-read-only (get-participant-info
        (cid uint)
        (participant principal)
    )
    (map-get? participants {
        circle-id: cid,
        participant: participant,
    })
)

(define-read-only (is-circle-active)
    (var-get current-circle-active)
)

(define-read-only (get-current-minimum)
    (var-get current-circle-minimum)
)

(define-read-only (get-my-recipient (cid uint))
    (match (map-get? participants {
        circle-id: cid,
        participant: tx-sender,
    })
        participant-data (ok (get recipient participant-data))
        err-not-participant
    )
)

(define-read-only (get-my-giver (cid uint))
    (match (map-get? participants {
        circle-id: cid,
        participant: tx-sender,
    })
        participant-data (ok (get giver participant-data))
        err-not-participant
    )
)

(define-read-only (get-participant-by-index
        (cid uint)
        (index uint)
    )
    (map-get? circle-participants {
        circle-id: cid,
        index: index,
    })
)

;; --- Private Functions ---

(define-private (get-recipient-index
        (cid uint)
        (total uint)
        (current-index uint)
    )
    (let (
            ;; Using block-height and matching-nonce for pseudo-randomness
            ;; In a real app, you might want more entropy or a VRF
            (random-offset (mod (+ (var-get matching-nonce) current-index block-height) total))
            (next-index (mod (+ current-index random-offset u1) total))
        )
        (if (is-eq next-index current-index)
            (mod (+ current-index u1) total)
            next-index
        )
    )
)

(define-private (match-single-participant
        (index uint)
        (context {
            cid: uint,
            total: uint,
        })
    )
    (let (
            (cid (get cid context))
            (total (get total context))
        )
        (if (< index total)
            (let (
                    (participant (unwrap-panic (map-get? circle-participants {
                        circle-id: cid,
                        index: index,
                    })))
                    (recipient-index (get-recipient-index cid total index))
                    (recipient (unwrap-panic (map-get? circle-participants {
                        circle-id: cid,
                        index: recipient-index,
                    })))
                    (participant-data (unwrap-panic (map-get? participants {
                        circle-id: cid,
                        participant: participant,
                    })))
                    (recipient-data (unwrap-panic (map-get? participants {
                        circle-id: cid,
                        participant: recipient,
                    })))
                )
                (map-set participants {
                    circle-id: cid,
                    participant: participant,
                }
                    (merge participant-data { recipient: (some recipient) })
                )
                (map-set participants {
                    circle-id: cid,
                    participant: recipient,
                }
                    (merge recipient-data { giver: (some participant) })
                )
                context
            )
            context
        )
    )
)

(define-private (perform-matching
        (cid uint)
        (count uint)
    )
    (begin
        (var-set matching-nonce (+ (var-get matching-nonce) u1))
        ;; Use fold to iterate through indices since recursion is not allowed
        (fold match-single-participant participant-indices {
            cid: cid,
            total: count,
        })
        true
    )
)

;; --- Public Functions ---

(define-public (create-circle (minimum uint))
    (let ((new-circle-id (+ (var-get circle-id) u1)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (>= minimum minimum-gift-amount) err-invalid-minimum)
        (asserts! (not (var-get current-circle-active)) err-circle-not-ready)

        (map-set circles new-circle-id {
            active: true,
            minimum-amount: minimum,
            participant-count: u0,
            matched: false,
            total-deposited: u0,
        })

        (var-set circle-id new-circle-id)
        (var-set current-circle-active true)
        (var-set current-circle-minimum minimum)

        (ok new-circle-id)
    )
)

(define-public (join-circle (amount uint))
    (let (
            (cid (var-get circle-id))
            (circle (unwrap! (map-get? circles cid) err-not-found))
            (current-count (get participant-count circle))
        )
        (asserts! (get active circle) err-circle-not-active)
        (asserts! (>= amount (get minimum-amount circle)) err-insufficient-amount)
        (asserts!
            (is-none (map-get? participants {
                circle-id: cid,
                participant: tx-sender,
            }))
            err-already-joined
        )

        ;; Transfer to contract address
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

        (map-set participants {
            circle-id: cid,
            participant: tx-sender,
        } {
            deposited: amount,
            recipient: none,
            giver: none,
            claimed: false,
        })

        (map-set circle-participants {
            circle-id: cid,
            index: current-count,
        }
            tx-sender
        )

        (map-set participant-index {
            circle-id: cid,
            participant: tx-sender,
        }
            current-count
        )

        (map-set circles cid
            (merge circle {
                participant-count: (+ current-count u1),
                total-deposited: (+ (get total-deposited circle) amount),
            })
        )

        (ok true)
    )
)

(define-public (match-participants)
    (let (
            (cid (var-get circle-id))
            (circle (unwrap! (map-get? circles cid) err-not-found))
            (participant-count (get participant-count circle))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (get active circle) err-circle-not-active)
        (asserts! (not (get matched circle)) err-already-matched)
        (asserts! (>= participant-count u2) err-circle-not-ready)

        (perform-matching cid participant-count)

        (map-set circles cid
            (merge circle {
                matched: true,
                active: false,
            })
        )

        (var-set current-circle-active false)

        (ok true)
    )
)

(define-public (claim-gift)
    (let (
            (cid (var-get circle-id))
            (participant-data (unwrap!
                (map-get? participants {
                    circle-id: cid,
                    participant: tx-sender,
                })
                err-not-participant
            ))
            (circle (unwrap! (map-get? circles cid) err-not-found))
            (giver (unwrap! (get giver participant-data) err-circle-not-ready))
            (giver-data (unwrap!
                (map-get? participants {
                    circle-id: cid,
                    participant: giver,
                })
                err-not-found
            ))
            (gift-amount (get deposited giver-data))
        )
        (asserts! (get matched circle) err-circle-not-ready)
        (asserts! (not (get claimed participant-data)) err-already-claimed)

        ;; Transfer from contract to recipient
        (let ((recipient tx-sender))
            (try! (as-contract (stx-transfer? gift-amount tx-sender recipient)))
        )

        (map-set participants {
            circle-id: cid,
            participant: tx-sender,
        }
            (merge participant-data { claimed: true })
        )

        (ok gift-amount)
    )
)
