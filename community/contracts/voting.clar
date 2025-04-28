;; Community Voting Contract
;; A robust implementation of a decentralized voting system on Stacks blockchain

;; Error codes
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_PROPOSAL_EXISTS (err u2))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u3))
(define-constant ERR_VOTING_CLOSED (err u4))
(define-constant ERR_ALREADY_VOTED (err u5))
(define-constant ERR_INSUFFICIENT_FUNDS (err u6))
(define-constant ERR_INVALID_VOTE (err u7))
(define-constant ERR_PROPOSAL_ACTIVE (err u8))
(define-constant ERR_ZERO_AMOUNT (err u9))
(define-constant ERR_INVALID_INPUT (err u10))
(define-constant ERR_INVALID_PROPOSAL_ID (err u11))
(define-constant ERR_MAX_BLOCK_HEIGHT (err u12))

;; Data storage
(define-map proposals
  { proposal-id: uint }
  {
    creator: principal,
    title: (string-utf8 256),
    description: (string-utf8 1024),
    start-block-height: uint,
    end-block-height: uint,
    status: (string-utf8 16),
    yes-votes: uint,
    no-votes: uint,
    abstain-votes: uint,
    executed: bool
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: (string-utf8 16), weight: uint }
)

(define-map user-stake
  { user: principal }
  { amount: uint }
)

(define-data-var proposal-count uint u0)
(define-data-var admin principal tx-sender)
(define-data-var min-proposal-duration uint u144) ;; Approximately 1 day in blocks (assuming 10 min blocks)
(define-data-var quorum-threshold uint u100) ;; Minimum votes required for a proposal to pass (adjusted based on stake)
(define-data-var current-block uint u0) ;; We'll use this as a mock for current block height
(define-data-var max-block-height uint u1000000) ;; Maximum allowed block height for safety

;; Read-only functions

;; Get the current block height (mock implementation)
(define-read-only (get-current-block-height)
  (var-get current-block)
)

;; Admin-only function to update the current block height (for testing purposes)
(define-public (set-current-block (new-height uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    ;; Validate the new height is reasonable
    (asserts! (< new-height (var-get max-block-height)) ERR_MAX_BLOCK_HEIGHT)
    (var-set current-block new-height)
    (ok true)
  )
)

;; Get the total number of proposals
(define-read-only (get-proposal-count)
  (var-get proposal-count)
)

;; Get details for a specific proposal
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Check if a user has voted on a specific proposal
(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? votes { proposal-id: proposal-id, voter: voter }))
)

;; Get a user's vote on a specific proposal
(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

;; Get a user's stake
(define-read-only (get-user-stake (user principal))
  (default-to { amount: u0 } (map-get? user-stake { user: user }))
)

;; Check if a proposal is active
(define-read-only (is-proposal-active (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (let ((current-block-height (var-get current-block))
                  (start-height (get start-block-height proposal))
                  (end-height (get end-block-height proposal)))
              (and (>= current-block-height start-height)
                   (<= current-block-height end-height)))
    false
  )
)

;; Calculate total votes for a proposal
(define-read-only (get-total-votes (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (+ (get yes-votes proposal) 
                (get no-votes proposal) 
                (get abstain-votes proposal))
    u0
  )
)

;; Calculate if a proposal has reached quorum
(define-read-only (has-reached-quorum (proposal-id uint))
  (>= (get-total-votes proposal-id) (var-get quorum-threshold))
)

;; Calculate if a proposal has passed (more yes than no votes and reached quorum)
(define-read-only (has-proposal-passed (proposal-id uint))
  (match (map-get? proposals { proposal-id: proposal-id })
    proposal (and (has-reached-quorum proposal-id)
                  (> (get yes-votes proposal) (get no-votes proposal)))
    false
  )
)

;; Check if proposal ID is valid
(define-read-only (is-valid-proposal-id (proposal-id uint))
  (< proposal-id (var-get proposal-count))
)

;; Public functions

;; Stake tokens to gain voting power
(define-public (stake (amount uint))
  (begin
    (asserts! (> amount u0) ERR_ZERO_AMOUNT)
    (let ((current-stake (get amount (get-user-stake tx-sender))))
      ;; In a real contract, we would transfer tokens here
      ;; (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (map-set user-stake 
              { user: tx-sender } 
              { amount: (+ current-stake amount) })
      (ok true)
    )
  )
)

;; Unstake tokens
(define-public (unstake (amount uint))
  (begin
    (asserts! (> amount u0) ERR_ZERO_AMOUNT)
    (let ((current-stake (get amount (get-user-stake tx-sender))))
      (asserts! (>= current-stake amount) ERR_INSUFFICIENT_FUNDS)
      ;; In a real contract, we would transfer tokens back here
      ;; (try! (as-contract (stx-transfer? amount (as-contract tx-sender) tx-sender)))
      (map-set user-stake 
              { user: tx-sender } 
              { amount: (- current-stake amount) })
      (ok true)
    )
  )
)

;; Create a new proposal
(define-public (create-proposal 
                (title (string-utf8 256)) 
                (description (string-utf8 1024)) 
                (duration uint))
  (let ((proposal-id (var-get proposal-count))
        (current-block-height (var-get current-block))
        (user-stake-amount (get amount (get-user-stake tx-sender))))
    
    ;; Ensure proposer has enough stake
    (asserts! (> user-stake-amount u0) ERR_INSUFFICIENT_FUNDS)
    
    ;; Ensure minimum proposal duration
    (asserts! (>= duration (var-get min-proposal-duration)) ERR_UNAUTHORIZED)
    
    ;; Ensure end block height doesn't overflow
    (asserts! (<= (+ current-block-height duration) (var-get max-block-height)) ERR_MAX_BLOCK_HEIGHT)
    
    ;; Create the proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        start-block-height: current-block-height,
        end-block-height: (+ current-block-height duration),
        status: u"active",
        yes-votes: u0,
        no-votes: u0,
        abstain-votes: u0,
        executed: false
      }
    )
    
    ;; Increment proposal counter
    (var-set proposal-count (+ proposal-id u1))
    
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote (proposal-id uint) (vote-type (string-utf8 16)))
  (let ((current-block-height (var-get current-block))
        (user-stake-amount (get amount (get-user-stake tx-sender))))
    
    ;; Validate proposal ID first
    (asserts! (is-valid-proposal-id proposal-id) ERR_INVALID_PROPOSAL_ID)
    
    ;; Check if proposal exists
    (asserts! (is-some (map-get? proposals { proposal-id: proposal-id })) 
              ERR_PROPOSAL_NOT_FOUND)
    
    ;; Get proposal details
    (let ((proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id }))))
      
      ;; Check if proposal is active
      (asserts! (and (>= current-block-height (get start-block-height proposal))
                    (<= current-block-height (get end-block-height proposal)))
                ERR_VOTING_CLOSED)
      
      ;; Check if user has already voted
      (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender }))
                ERR_ALREADY_VOTED)
      
      ;; Check if user has stake
      (asserts! (> user-stake-amount u0) ERR_INSUFFICIENT_FUNDS)
      
      ;; Check if vote type is valid
      (asserts! (or (is-eq vote-type u"yes") 
                    (is-eq vote-type u"no") 
                    (is-eq vote-type u"abstain"))
                ERR_INVALID_VOTE)
      
      ;; Record the vote
      (map-set votes
        { proposal-id: proposal-id, voter: tx-sender }
        { vote: vote-type, weight: user-stake-amount }
      )
      
      ;; Update vote counts
      (if (is-eq vote-type u"yes")
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal { yes-votes: (+ (get yes-votes proposal) user-stake-amount) })
        )
        (if (is-eq vote-type u"no")
          (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { no-votes: (+ (get no-votes proposal) user-stake-amount) })
          )
          ;; Must be "abstain"
          (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { abstain-votes: (+ (get abstain-votes proposal) user-stake-amount) })
          )
        )
      )
      
      (ok true)
    )
  )
)

;; Execute a proposal that has passed
(define-public (execute-proposal (proposal-id uint))
  (let ((current-block-height (var-get current-block)))
    
    ;; Validate proposal ID first
    (asserts! (is-valid-proposal-id proposal-id) ERR_INVALID_PROPOSAL_ID)
    
    ;; Check if proposal exists
    (asserts! (is-some (map-get? proposals { proposal-id: proposal-id })) 
              ERR_PROPOSAL_NOT_FOUND)
    
    ;; Get proposal details
    (let ((proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id }))))
      
      ;; Check if voting has ended
      (asserts! (> current-block-height (get end-block-height proposal))
                ERR_PROPOSAL_ACTIVE)
      
      ;; Check if proposal has passed
      (asserts! (has-proposal-passed proposal-id) ERR_UNAUTHORIZED)
      
      ;; Check if proposal has already been executed
      (asserts! (not (get executed proposal)) ERR_UNAUTHORIZED)
      
      ;; Mark proposal as executed
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { 
          status: u"executed",
          executed: true
        })
      )
      
      ;; In a real contract, we would execute the proposal's action here
      ;; This might involve calling other contracts, transferring funds, etc.
      
      (ok true)
    )
  )
)

;; Cancel a proposal (only creator or admin can cancel)
(define-public (cancel-proposal (proposal-id uint))
  (begin
    ;; Validate proposal ID first
    (asserts! (is-valid-proposal-id proposal-id) ERR_INVALID_PROPOSAL_ID)
    
    ;; Check if proposal exists
    (asserts! (is-some (map-get? proposals { proposal-id: proposal-id })) 
              ERR_PROPOSAL_NOT_FOUND)
    
    ;; Get proposal details
    (let ((proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id }))))
      
      ;; Check if caller is authorized (creator or admin)
      (asserts! (or (is-eq tx-sender (get creator proposal))
                    (is-eq tx-sender (var-get admin)))
                ERR_UNAUTHORIZED)
      
      ;; Mark proposal as canceled
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { 
          status: u"canceled"
        })
      )
      
      (ok true)
    )
  )
)

;; Admin functions

;; Change the admin (only current admin can do this)
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    ;; No need for additional validation on principal type
    (var-set admin new-admin)
    (ok true)
  )
)

;; Update the minimum proposal duration
(define-public (set-min-proposal-duration (new-duration uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    ;; Validate that duration is reasonable
    (asserts! (> new-duration u0) ERR_INVALID_INPUT)
    (var-set min-proposal-duration new-duration)
    (ok true)
  )
)

;; Update the quorum threshold
(define-public (set-quorum-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    ;; Validate the threshold is reasonable
    (asserts! (> new-threshold u0) ERR_INVALID_INPUT)
    (var-set quorum-threshold new-threshold)
    (ok true)
  )
)

;; Update the max block height (admin only)
(define-public (set-max-block-height (new-max uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR_UNAUTHORIZED)
    ;; Validate it's greater than current block
    (asserts! (> new-max (var-get current-block)) ERR_INVALID_INPUT)
    (var-set max-block-height new-max)
    (ok true)
  )
)