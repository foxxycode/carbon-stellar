;; CarbonStellar - Decentralized Carbon Credit Trading Platform
;; A community-verified carbon sequestration project management system

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-already-verified (err u105))
(define-constant err-not-verified (err u106))
(define-constant err-milestone-not-reached (err u107))

;; Data Variables
(define-data-var platform-fee-percent uint u2) ;; 2% platform fee
(define-data-var total-carbon-credits uint u0)
(define-data-var project-nonce uint u0)

;; Data Maps
(define-map projects
    uint
    {
        owner: principal,
        name: (string-ascii 100),
        location: (string-ascii 100),
        carbon-credits: uint,
        price-per-credit: uint,
        verified: bool,
        verification-count: uint,
        required-verifications: uint,
        milestone-reached: bool,
        created-at: uint
    }
)

(define-map verifiers
    principal
    {
        reputation-score: uint,
        total-verifications: uint,
        governance-tokens: uint
    }
)

(define-map project-verifications
    {project-id: uint, verifier: principal}
    {verified: bool, timestamp: uint}
)

(define-map carbon-credit-balances
    principal
    uint
)

(define-map project-milestones
    {project-id: uint, milestone-id: uint}
    {
        description: (string-ascii 200),
        carbon-target: uint,
        payment-amount: uint,
        completed: bool
    }
)

;; Read-only functions
(define-read-only (get-project (project-id uint))
    (map-get? projects project-id)
)

(define-read-only (get-verifier-info (verifier principal))
    (map-get? verifiers verifier)
)

(define-read-only (get-carbon-balance (user principal))
    (default-to u0 (map-get? carbon-credit-balances user))
)

(define-read-only (get-platform-fee)
    (var-get platform-fee-percent)
)

(define-read-only (get-total-credits)
    (var-get total-carbon-credits)
)

(define-read-only (has-verified-project (project-id uint) (verifier principal))
    (match (map-get? project-verifications {project-id: project-id, verifier: verifier})
        verification (get verified verification)
        false
    )
)

;; Private functions
(define-private (calculate-fee (amount uint))
    (/ (* amount (var-get platform-fee-percent)) u100)
)

;; Public functions

;; Register a new carbon sequestration project
(define-public (create-project 
    (name (string-ascii 100))
    (location (string-ascii 100))
    (carbon-credits uint)
    (price-per-credit uint)
    (required-verifications uint))
    (let
        (
            (new-project-id (+ (var-get project-nonce) u1))
        )
        (asserts! (> carbon-credits u0) err-invalid-amount)
        (asserts! (> price-per-credit u0) err-invalid-amount)
        (asserts! (> required-verifications u0) err-invalid-amount)
        
        (map-set projects new-project-id
            {
                owner: tx-sender,
                name: name,
                location: location,
                carbon-credits: carbon-credits,
                price-per-credit: price-per-credit,
                verified: false,
                verification-count: u0,
                required-verifications: required-verifications,
                milestone-reached: false,
                created-at: block-height
            }
        )
        
        (var-set project-nonce new-project-id)
        (ok new-project-id)
    )
)

;; Community verification - validators verify project impact
(define-public (verify-project (project-id uint))
    (let
        (
            (project (unwrap! (map-get? projects project-id) err-not-found))
            (verifier-info (default-to 
                {reputation-score: u0, total-verifications: u0, governance-tokens: u0}
                (map-get? verifiers tx-sender)))
            (already-verified (has-verified-project project-id tx-sender))
        )
        (asserts! (not already-verified) err-already-verified)
        (asserts! (not (get verified project)) err-already-verified)
        
        ;; Record verification
        (map-set project-verifications 
            {project-id: project-id, verifier: tx-sender}
            {verified: true, timestamp: block-height}
        )
        
        ;; Update project verification count
        (let
            (
                (new-verification-count (+ (get verification-count project) u1))
                (is-now-verified (>= new-verification-count (get required-verifications project)))
            )
            (map-set projects project-id
                (merge project {
                    verification-count: new-verification-count,
                    verified: is-now-verified
                })
            )
            
            ;; Reward verifier with governance tokens
            (map-set verifiers tx-sender
                {
                    reputation-score: (+ (get reputation-score verifier-info) u10),
                    total-verifications: (+ (get total-verifications verifier-info) u1),
                    governance-tokens: (+ (get governance-tokens verifier-info) u5)
                }
            )
            
            (if is-now-verified
                (var-set total-carbon-credits (+ (var-get total-carbon-credits) (get carbon-credits project)))
                true
            )
            
            (ok is-now-verified)
        )
    )
)

;; Purchase carbon credits
(define-public (purchase-credits (project-id uint) (amount uint))
    (let
        (
            (project (unwrap! (map-get? projects project-id) err-not-found))
            (total-cost (* amount (get price-per-credit project)))
            (fee (calculate-fee total-cost))
            (net-amount (- total-cost fee))
            (buyer-balance (get-carbon-balance tx-sender))
        )
        (asserts! (get verified project) err-not-verified)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (<= amount (get carbon-credits project)) err-insufficient-balance)
        
        ;; Transfer payment to project owner (minus fee)
        (try! (stx-transfer? net-amount tx-sender (get owner project)))
        
        ;; Transfer fee to contract owner
        (try! (stx-transfer? fee tx-sender contract-owner))
        
        ;; Update project carbon credits
        (map-set projects project-id
            (merge project {
                carbon-credits: (- (get carbon-credits project) amount)
            })
        )
        
        ;; Update buyer's carbon credit balance
        (map-set carbon-credit-balances tx-sender (+ buyer-balance amount))
        
        (ok amount)
    )
)

;; Retire/offset carbon credits
(define-public (offset-emissions (amount uint))
    (let
        (
            (balance (get-carbon-balance tx-sender))
        )
        (asserts! (>= balance amount) err-insufficient-balance)
        (asserts! (> amount u0) err-invalid-amount)
        
        (map-set carbon-credit-balances tx-sender (- balance amount))
        (var-set total-carbon-credits (- (var-get total-carbon-credits) amount))
        
        (ok amount)
    )
)

;; Create milestone for a project
(define-public (create-milestone 
    (project-id uint)
    (milestone-id uint)
    (description (string-ascii 200))
    (carbon-target uint)
    (payment-amount uint))
    (let
        (
            (project (unwrap! (map-get? projects project-id) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner project)) err-unauthorized)
        
        (map-set project-milestones
            {project-id: project-id, milestone-id: milestone-id}
            {
                description: description,
                carbon-target: carbon-target,
                payment-amount: payment-amount,
                completed: false
            }
        )
        
        (ok true)
    )
)

;; Complete milestone and release payment
(define-public (complete-milestone (project-id uint) (milestone-id uint))
    (let
        (
            (project (unwrap! (map-get? projects project-id) err-not-found))
            (milestone (unwrap! (map-get? project-milestones {project-id: project-id, milestone-id: milestone-id}) err-not-found))
        )
        (asserts! (get verified project) err-not-verified)
        (asserts! (not (get completed milestone)) err-already-verified)
        
        ;; Mark milestone as completed
        (map-set project-milestones
            {project-id: project-id, milestone-id: milestone-id}
            (merge milestone {completed: true})
        )
        
        ;; Release payment to project owner
        (try! (stx-transfer? (get payment-amount milestone) contract-owner (get owner project)))
        
        (ok true)
    )
)

;; Transfer carbon credits between users
(define-public (transfer-credits (amount uint) (recipient principal))
    (let
        (
            (sender-balance (get-carbon-balance tx-sender))
            (recipient-balance (get-carbon-balance recipient))
        )
        (asserts! (>= sender-balance amount) err-insufficient-balance)
        (asserts! (> amount u0) err-invalid-amount)
        
        (map-set carbon-credit-balances tx-sender (- sender-balance amount))
        (map-set carbon-credit-balances recipient (+ recipient-balance amount))
        
        (ok amount)
    )
)

;; Admin function to update platform fee
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u10) err-invalid-amount) ;; Max 10% fee
        (var-set platform-fee-percent new-fee)
        (ok true)
    )
)