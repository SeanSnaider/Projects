(require 2htdp/image)
(require 2htdp/universe)

; A HierarchicalBitmapSet (HBS) is a [List-of [List-of Boolean]]

(define-struct hbs-alloc [block hbs])
; A HBSAllocResult is a (make-hbs-alloc Integer HBS)
; Represents the result of an allocation call, where:
; - block is the starting block number for the chunk that was allocated;
;   or -1 if no space available
; - hbs is the updated HBS after the allocation
(define HBSAR-1 (make-hbs-alloc 2
                                (list (list #f #t)
                                      (list #t #f #f #f))))
(define (hbsar-temp hbsar)
  (... (hbs-alloc-block hbsar) ...
       (hbs-temp (hbs-alloc-hbs hbsar) ...)))


; A ListOfBoolPairs (LoBP) is one of:
; - empty
; - (cons Boolean (cons Boolean ListOfBoolPairs))
; Represents an empty list, or the first pair of Booleans in an
; even-length list of Bools,
(define LOBP-0 '())
(define LOBP-1 (list #false #false))
(define LOBP-2 (append (list #true #true) LOBP-1))
(define (lobp-temp lobp)
  (...
   (cond [(empty? lobp) ...]
         [(cons? lobp) (... (first lobp) ...
                            (first (rest lobp)) ...
                            (lobp-temp (rest (rest lobp))) ...)])))

;Exercise 1

;Part A

;Signature: blocks-remaining: hbs -> natNum

;Interpretation
; Takes in an HBS and then will reutrn the number of free spaces
; there are on the drive. In essence, it will count the trues

;tests

(check-expect (blocks-remaining
               (list (list #true #false)
                     (list #false #false #true #false)
                     (list #false #false #false #false #false #false #true #false))) 7)
(check-expect (blocks-remaining
               (list (list #true #false)
                     (list #false #false #true #false)
                     (list #false #false #false #false #false #false #false #false))) 6)
(check-expect (blocks-remaining
               (list (list #true #true)
                     (list #false #false #false #false)
                     (list #false #false #false #false #false #false #false #false))) 8)
(check-expect (blocks-remaining
               (list (list #false #false)
                     (list #false #false #false #false)
                     (list #false #false #false #false #false #false #false #false))) 0)

;code

(define (blocks-remaining hbs)
  (cond
    [(empty? hbs) 0]
    [(cons? (first hbs))
     (local [(define (add-chunks x)
               (cond
                 [(empty? x) 0]
                 [(and (cons? x) (first x))
                  (+ (expt 2 (- (length hbs) 1)) (add-chunks (rest x)))]
                 [(and (cons? x) (not (first x)))
                  (+ 0 (add-chunks (rest x)))]))]
       (+ (add-chunks (first hbs))
          (blocks-remaining (rest hbs))))]))

;Part B

;Signature: find-chunk: hbs nat -> Num

;Interpretation
; Takes in a HBS and a chunk size and then will determine
; the opening that will be allocated to a given data of
; that chunk size then returning the first index of such

;tests

(check-expect (find-chunk
               (list (list #true #false)
                     (list #false #false #false #false)
                     (list #false #false #false #false #false #false #false #false))
               2) 0)
(check-expect (find-chunk
               (list (list #true #false)
                     (list #false #false #false #false)
                     (list #false #false #false #false #false #false #false #false))
               4) 0)
(check-expect (find-chunk
               (list (list #false #true)
                     (list #false #false #false #false)
                     (list #false #false #false #false #false #false #false #false))
               4) 4)
(check-expect (find-chunk
               (list (list #false #true)
                     (list #false #false #false #false)
                     (list #true #false #true #false #false #false #false #false))
               2) 4)
(check-expect (find-chunk
               (list (list #false #false)
                     (list #true #false #false #false)
                     (list #false #false #false #false #false #false #false #false))
               2) 0)



;code

(define (find-chunk hbs size)
  (if (<= size (expt 2 (sub1 (length hbs)))) 
      (first-chunk (map first-true hbs) 1 (- (length hbs) 1) size)
      -1))
  


;Signature: first-true: [List of Booleans] -> integer

;Interpretation;
; Will take in a list of booleans and loop through it to find where the first true is.
; When teh first true is found, it will return the index that the true occured at
; if no true is found, -1 is returned.

;tests

(check-expect (first-true (list #t #f #f #f #t)) 0)
(check-expect (first-true (list #f #f #f #f #f)) -1)
(check-expect (first-true (list #f #f #t #f #f)) 2)

;code

(define (first-true lob)
  (cond [(empty? lob) -1]
        [(cons? lob) (if (first lob)
                         0
                         (if (= -1 (first-true (rest lob)))
                             -1
                             (add1 (first-true (rest lob)))))]))


;signature first-chunk: [List of Integers] natNum integer integer-> Integer

;Interpretation
; Takes in a list of numbers, these numbers being either -1 if there was no true
; within that level of the hbs, or the index of the first true within the hbs. using that,
; it will check which of these indexes, if the size is large enough, is appropriate to
; fit a chunk. The first integer represents the size, which starts at 1 and increases as we
; move up the list The last integer represents the spot of the list, starting at the end
; of the list, moving to the front so that we dont allocate too large of a chunk. In addition
; the last integer represents the size that we would be looking for

(check-expect (first-chunk (list -1 -1 2 3 0) 1 4 4)
              8)
(check-expect (first-chunk (list -1 -1 2 3 0) 1 4 2)
              6)
(check-expect (first-chunk (list -1 -1 -1 -1 -1) 1 4 2)
              -1)

;code

(define (first-chunk lon chunk-size spot n)
  (if (>= spot 0)
      (if (and (> (get-element lon spot) -1) (>= chunk-size n))
          (* (get-element lon spot) chunk-size)
          (first-chunk lon (* chunk-size 2) (- spot 1) n))
      -1))


;Signature: get-element: (x) [List of x] nat -> x

;Interpretation
; Takes in a list of anything and then takes an index of the list
; then returning the value at that index of the list

;tests

(check-expect (get-element (list 2 2 2 2 2 4) 3) 2)
(check-expect (get-element (list 1 2 3 4 5 6) 4) 5)
(check-expect (get-element (list 0 2 3 4 5 1) 0) 0)

;code

(define (get-element lox n)
  (cond
    [(empty? lox) "Index out of range"]
    [(cons? lox)
     (cond
       [(= n 0)
        (first lox)]
       [else
        (get-element (rest lox) (- n 1))])]))

;Exercise 1C

;signature: initialize-hbs : [List-of Boolean] -> [List-of [List-of Boolean]]

;Interpretation: will take in a list of booleans that represents the bottom most layer of a HBS
; the program will then return the HBS for said list in proper normal form

;tests

(check-expect (initialize-hbs
               (list #false #false #true #true #true #false #false #false))
              (list (list #false #false)
                    (list #false #true #false #false)
                    (list #false #false #false #false #true #false #false #false)))
(check-expect (initialize-hbs
               (list #true #true #true #true #true #false #false #false))
              (list (list #true #false)
                    (list #false #false #false #false)
                    (list #false #false #false #false #true #false #false #false)))
(check-expect (initialize-hbs
               (list #true #true #true #true #true #false #true #true))
              (list (list #true #false)
                    (list #false #false #false #true)
                    (list #false #false #false #false #true #false #false #false)))
(check-expect (initialize-hbs
               (list #true #true #true #true #true #true #true #true
                     #false #true #false #true #false #true #false #true))
              (list (list #true #false)
                    (list #false #false #false #false)
                    (list #false #false #false #false #false #false #false #false)
                    (list #false #false #false #false #false #false #false #false
                          #false #true #false #true #false #true #false #true)))
                    

;code


(define (initialize-hbs lobp)
  (local [(define (move-up next-list prev-list)          
            (cond
              [(empty? next-list) '()]
              [(cons? next-list)
               (if (> (length (make-new-list next-list)) 2)
                   (append (move-up (make-new-list next-list) next-list)
                           (list (normalize prev-list)))
                   (append (list (make-new-list next-list))
                           (list (normalize (make-new-list prev-list)))
                           (list (normalize prev-list))))]))]
    (move-up (make-new-list lobp) lobp)))


;signature: make-new-list: [List of BooleanPairs] -> [List of BooleanPairs]

;Interpretation
; Will take a list of boolean pairs and then, according to how that list is set up,
; creates the next appropriate list in the hbs

;tests

(check-expect (make-new-list (list #t #t #t #t #f #f #t #t))
              (list #t #t #f #t))

(check-expect (make-new-list (list #t #f #t #t #f #f #t #t))
              (list #f #t #f #t))

(check-expect (make-new-list (list #t #f #f #t #f #f #t #t))
              (list #f #f #f #t))

;code

(define (make-new-list lobp)
  (cond
    [(empty? lobp) lobp]
    [(cons? lobp)
     (if (and (first (rest lobp)) (first lobp))  
         (cons #true (make-new-list (rest (rest lobp))))
         (cons  #false (make-new-list (rest (rest lobp)))))]))
       
;signature: normalize: [List of BooleanPairs] -> [List of BooleanPairs]

;Interpretation
; Takes in a list of boolean pairs and then will normalize it so that the
; upper lsit will have the necessary parts
; to normalize the total list

;tests

(check-expect (normalize (list #true #true #true #false #true #true #true #false))
              (list #false #false #true #false #false #false #true #false))

(check-expect (normalize (list #true #true #true #true #true #true #true #true))
              (list #false #false #false #false #false #false #false #false))

(check-expect (normalize (list #false #true #false #false #true #true #true #false))
              (list #false #true #false #false #false #false #true #false))

;code

(define (normalize lobp)
  (cond
    [(or (empty? lobp) (empty? (rest lobp))) '()]
    [(cons? lobp)
     (if (and (first (rest lobp)) (first lobp))
         (cons #false (cons #false (normalize (rest (rest lobp)))))
         (cons (first lobp) (cons (first (rest lobp)) (normalize (rest (rest lobp))))))]))

  
;Signature: set-false: List-of-boolean NatNum -> List-of-Boolean

;Interpretation
; Takes in a list and an index, then changing the value at that given index to false.

;tests

(check-expect (set-false (list #true #true #false #false) 1)
              (list #true #false #false #false))

(check-expect (set-false (list #true #true #false #false) 3)
              (list #true #true #false #false))

(check-expect (set-false (list #true #true #false #false) 0)
              (list #false #true #false #false))

;code

(define (set-false lob n)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (zero? n)
                         (cons #false (rest lob))
                         (cons (first lob) (set-false (rest lob) (sub1 n))))]))

;Exercise 2

;Allocation

;Signature: alloc-chunk: HBS NatNum -> HBSAllocResult

;Interpretation:
; this takes in a HBS and a NatNum that represents the chunk size,
; then allocating that chunk in the HBS and returning an HBSAllocResult
; which is a struct of the starting block where the data is allocated and
; the HBS that was used (with the necessary edits)

;check-expects

(check-expect (alloc-chunk
               (list (list #true #false)
                     (list #false #false #false #true)
                     (list #false #false #false #false #true #false #false #false))
               2)
              (make-hbs-alloc 6
                              (list (list #true #false)
                                    (list #false #false #false #false)
                                    (list #false #false #false #false #true #false #false #false))))



(check-expect (alloc-chunk
               (list (list #true #false)
                     (list #false #false #false #true)
                     (list #false #false #false #false #true #false #false #false))
               4)
              (make-hbs-alloc 0
                              (list (list #false #false)
                                    (list #false #false #false #true)
                                    (list #false #false #false #false #true #false #false #false))))


(check-expect (alloc-chunk
               (list (list #false #false)
                     (list #false #true #false #true)
                     (list #false #false #false #false #true #false #false #false))
               2)
              (make-hbs-alloc 2
                              (list (list #false #false)
                                    (list #false #false #false #true)
                                    (list #false #false #false #false #true #false #false #false))))
                              

;code

(define (alloc-chunk hbs n)
  (local [(define block (find-chunk hbs n))]
    (if (< block 0)
        (make-hbs-alloc -1 hbs)
        (make-hbs-alloc block (check-chunk hbs n block)))))
        
          
;another helper

;signature: check-chunk: hbs Integer Integer -> hbs

;Interpretation:
; takes in an hbs, an integer representing size, and an integer which is the
; index of the bottom most layer allocation. The function will
; go through the hbs, check each given index and see if we are either, greater
; than, less than, or equal to to the size at the current level we are at.
; if we are greater than, we return hbs, if we are equal to, we set the value
; to false and add it to the list, and if it is less than we check to see if the
; index of that list at that input is true, and if it is we set it to false and then
; recurse through the function once more while calling a helper to set the next
; two under the function to true.

;tests

(check-expect (check-chunk
               (list (list #false #false)
                     (list #false #true #false #true)
                     (list #false #false #false #false #true #false #false #false))
               2 2)
              (list (list #false #false)
                    (list #false #false #false #true)
                    (list #false #false #false #false #true #false #false #false)))

(check-expect (check-chunk
               (list (list #true #false)
                     (list #false #false #false #true)
                     (list #false #false #false #false #true #false #false #false))
               4 0)
              (list (list #false #false)
                    (list #false #false #false #true)
                    (list #false #false #false #false #true #false #false #false)))

(check-expect (check-chunk
               (list (list #true #false)
                     (list #false #false #true #false)
                     (list #false #false #false #false #false #false #true #false))
               2 4)
              (list (list #true #false)
                    (list #false #false #false #false)
                    (list #false #false #false #false #false #false #true #false)))

(check-expect (check-chunk
               (list (list #true #false)
                     (list #false #false #true #false)
                     (list #false #false #false #false #false #false #true #false))
               1 6)
              (list (list #true #false)
                    (list #false #false #true #false)
                    (list #false #false #false #false #false #false #false #false)))

                          

;code
              
(define (check-chunk hbs n block)
  (local [(define get-chunk-index (floor (/ block (expt 2 (sub1 (length hbs))))))]
    (cond
      [(empty? hbs) hbs]
      [(cons? hbs)
       (cond
         [(> n (expt 2 (sub1 (length hbs))))
          hbs]
         [(= n (expt 2 (sub1 (length hbs))))
          (cons (set-false (first hbs) get-chunk-index) (rest hbs))]
         [(< n (expt 2 (sub1 (length hbs))))
          (if (nth-is-true? (first hbs) get-chunk-index)
              (cons (set-false (first hbs) get-chunk-index)
                    (check-chunk
                     (cons (check-pair (first (rest hbs)) get-chunk-index)
                           (rest (rest hbs)))
                     n block))
              (cons (first hbs) (check-chunk
                                 (rest hbs) n block)))])])))
    
         


;Signature: check-pair list-of-boolean-pairs natNum -> list-of-booleans

;Interpretation
; will, utilizing a true which will be checked for in a previous function, determine if the list of
; booleans the pair of booleans that is represeneted by that true needs to be changed
; and will change it as necessary, will also look specifically at the necessary index by
; looping through the whole list until it gets to the necessary point

;check-expects

(check-expect (check-pair
               (list #false #false #false #false)
               2)
              (list #false #false #true #true))

(check-expect (check-pair
               (list #false #false #true #false)
               0)
              (list #true #true #true #false))

(check-expect (check-pair
               (list #false #false #false #false #false #false #false #false)
               4)
              (list #false #false #false #false #true #true #false #false))


;code

(define (check-pair lobp index)
  (cond
    [(empty? lobp) '()]
    [(cons? lobp)
     (if (or (= index 0) (= index -1))
         (append (list #true) (check-pair (rest lobp) (- index 1)))
         (append (list (first lobp)) (check-pair (rest lobp) (- index 1))))]))

         

;Exercise 3

;Freeing up space

;Signature: nth-is-true? [List of BooleanPairs] Integer -> Boolean

;Interpretation:
; takes in a list of booleanPairs, and then checks to see if the index represented
; by the integer paramter is true. If it is, return true, otherwise return false.

;tests

(check-expect (nth-is-true? (list #t #t #f #t #f) 0) #t)
(check-expect (nth-is-true? (list #t #t #f #t #f) 2) #f)
(check-expect (nth-is-true? (list #t #t #f #t #f) 3) #t)

;code

(define (nth-is-true? lob n)
  (cond [(empty? lob) #false]
        [(cons? lob) (if (zero? n)
                         (first lob)
                         (nth-is-true? (rest lob) (sub1 n)))]))

;Signature: free-chunk: hbs natNum integer -> hbs

;Interpretation:
; takes in an hbs, a natural representing the size of the chunk, and an integer
; representing the starting block of the chunk then removes the allocation that was put there,
; freeing up the space and normalizing the list once more then returning the produced hbs

;tests

(check-expect (free-chunk
               (list (list #true #false)
                     (list #false #false #false #false)
                     (list #false #false #false #false #true #false #false #false))
               2 6)
              (list (list #true #false)
                    (list #false #false #false #true)
                    (list #false #false #false #false #true #false #false #false)))


(check-expect (free-chunk
               (list (list #false #false)
                     (list #false #false #false #true)
                     (list #false #false #false #false #true #false #false #false))
               4 0)
              (list (list #true #false)
                    (list #false #false #false #true)
                    (list #false #false #false #false #true #false #false #false)))


(check-expect (free-chunk
               (list (list #false #false)
                     (list #false #false #false #true)
                     (list #false #false #false #false #true #false #false #false))
               2 2)
              (list (list #false #false)
                    (list #false #true #false #true)
                    (list #false #false #false #false #true #false #false #false)))

;code

(define (free-chunk hbs size allocation)
  (normalize-freeing (set-true-at-level hbs size allocation)
                     (first (set-true-at-level hbs size allocation))
                     (first (rest (set-true-at-level hbs size allocation)))
                     size))


;Signature: set-true-at-level: hbs natNum integer -> hbs

;Interpretation:
; takes in an hbs, a natNum representing the size of the chunk we are changing
; and an integer representing the index of the said chunk at the lowest level. This
; function takes in those conditions and then changes that singular value to be true
; at the lowest possible level

;tests

(check-expect (set-true-at-level
               (list (list #f #f)
                     (list #t #f #t #f)
                     (list #f #f #f #f #f #f #f #t))
               2 2)
              (list (list #f #f)
                    (list #t #t #t #f)
                    (list #f #f #f #f #f #f #f #t)))

(check-expect (set-true-at-level
               (list (list #f #f)
                     (list #t #f #t #f)
                     (list #f #f #f #f #f #f #f #t))
               1 2)
              (list (list #f #f)
                    (list #t #f #t #f)
                    (list #f #f #t #f #f #f #f #t)))

(check-expect (set-true-at-level
               (list (list #f #f)
                     (list #f #f #t #f)
                     (list #f #f #f #f #f #f #f #t))
               4 0)
              (list (list #t #f)
                    (list #f #f #t #f)
                    (list #f #f #f #f #f #f #f #t)))

;code

(define (set-true-at-level hbs n block)
  (local [(define get-chunk-index (floor (/ block (expt 2 (sub1 (length hbs))))))]
    (cond
      [(empty? hbs) hbs]
      [(cons? hbs)
       (cond
         [(> n (expt 2 (sub1 (length hbs))))
          hbs]
         [(= n (expt 2 (sub1 (length hbs))))
          (cons (set-true (first hbs) get-chunk-index) (rest hbs))]
         [(< n (expt 2 (sub1 (length hbs))))
          (cons (first hbs)
                (set-true-at-level (rest hbs) n block))])])))


;signature: normalize-freeing: hbs [List of booleanPairs] [List of booleanPairs] integer -> hbs

;Interpretation
; Takes in an hbs that has had its value set to true at the given chunk
; and then takes the LOBP and moves all trues down to the bottom layer,
; then calling on intialize-hbs to create a new hbs with the appropriate
; normal form.

;tests

(check-expect (normalize-freeing
               (list (list #t #f)
                     (list #f #f #t #t)
                     (list #f #f #f #f #f #f #f #f))
               (list #t #f)
               (list #f #f #t #t)
               2)
              (list (list #t #t)
                    (list #f #f #f #f)
                    (list #f #f #f #f #f #f #f #f)))

(check-expect (normalize-freeing
               (list (list #f #f)
                     (list #t #f #t #f)
                     (list #f #f #f #f #f #f #f #f))
               (list #f #f)
               (list #t #f #t #f)
               2)
              (list (list #f #f)
                    (list #t #f #t #f)
                    (list #f #f #f #f #f #f #f #f)))

(check-expect (normalize-freeing
               (list (list #f #f)
                     (list #t #t #f #t)
                     (list #f #f #f #f #f #f #f #f))
               (list #f #f)
               (list #t #t #f #t)
               2)
              (list (list #t #f)
                    (list #f #f #f #t)
                    (list #f #f #f #f #f #f #f #f)))

(check-expect (normalize-freeing
               (list (list #t #f)
                     (list #f #f #f #f)
                     (list #f #f #f #f #t #t #f #f)
                     (list #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #t))
               (list #t #f)
               (list #f #f #f #f)
               2)
              (list (list #t #f)
                    (list #f #f #t #f)
                    (list #f #f #f #f #f #f #f #f)
                    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #t)))

(check-expect (normalize-freeing
               (list (list #t #f)
                     (list #f #f #f #f)
                     (list #f #f #f #f #f #f #t #f)
                     (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #t))
               (list #t #f)
               (list #f #f #f #f)
               1)
              (list (list #t #f)
                    (list #f #f #f #t)
                    (list #f #f #f #f #f #f #f #f)
                    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))

;code

(define (normalize-freeing hbs current-list next-list size)
  (local [(define (move-all-trues-down hb current next)
            (cond
              [(empty? hb) hb]
              [(cons? hb)
               (if (> (length hb) 2)
                   (move-all-trues-down (rest hb)
                                        (map my-or (move-true-down current) next)
                                        (first (rest (rest hb))))
                   (map my-or (move-true-down current) next))]))]
    (initialize-hbs (move-all-trues-down hbs current-list next-list))))



;signature: move-true-down: lobp -> lobp

;Interpretation:
; Takes in a lobp that may have a true and then will create a new list
; that will move that true down to the next level as two trues

;tests

(check-expect (move-true-down
               (list #t #f #t #f))
              (list #t #t #f #f #t #t #f #f))

(check-expect (move-true-down
               (list #f #f #t #f))
              (list #f #f #f #f #t #t #f #f))

(check-expect (move-true-down
               (list #t #t #t #f))
              (list #t #t #t #t #t #t #f #f))
(check-expect (move-true-down
               (list #t #t))
              (list #t #t #t #t))
;code

(define (move-true-down lobp)
  (cond
    [(empty? lobp) '()]
    [(cons? lobp)
     (if (first lobp)
         (cons #true
               (cons #true (move-true-down (rest lobp))))
         (cons #false
               (cons #false (move-true-down (rest lobp)))))]))


;Signature: set-true : [List-of Boolean] NatNum -> [List-of Boolean]

;Interpretation
; returns the list with the n-th item in the list converted
; from #false to #true

;tests

(check-expect (set-true (list #t #t #f #f) 2)
              (list #t #t #t #f))

(check-expect (set-true (list #t #t #f #f) 1)
              (list #t #t #f #f))

(check-expect (set-true (list #t #t #f #f) 3)
              (list #t #t #f #t))

;code
              
(define (set-true lob n)
  (cond [(empty? lob) '()]
        [(cons? lob) (if (zero? n)
                         (cons #true (rest lob))
                         (cons (first lob) (set-true (rest lob) (sub1 n))))]))




;Signature; my-or: boolean boolean -> boolean

;Interpretation:
; Has the same functionality as or, will, given two inputs,
; return either the first or the second. In this case we will be doing booleans
; so if either the first or second value is true, return true, otherwise, return
; false if both values are false.

;tests

(check-expect (my-or #true #false) #true)
(check-expect (my-or #false #false) #false)
(check-expect (my-or #true #true) #true)
(check-expect (my-or #false #true) #true)

;code

(define (my-or b1 b2)
  (or b1 b2))
