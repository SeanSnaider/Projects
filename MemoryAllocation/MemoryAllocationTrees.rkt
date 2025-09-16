;HW 10

(require 2htdp/universe)
(require 2htdp/image)

(define-struct hbs [bit left right])
; A HierarchicalBitmapSet (HBS) is one of:
; - #false
; - (make-hbs Boolean HBS HBS)
; representing a bit at some level of a hierarchical tree of bits,
; or a sentinel value representing an empty tree/subtree
(define (hbs-temp hbs)
  (...
   (cond [(boolean? hbs) ...]
         [(hbs? hbs) (... (hbs-bit hbs) ...
                          (hbs-temp (hbs-left hbs)) ...
                          (hbs-temp (hbs-right hbs)) ...)])))



(define-struct hbs-chunk [block size])
; A HBSChunk is one of:
; - #false
; - (make-hbs-chunk NatNum PosInt)
; Represents the result of a chunk query, where:
; - block is the starting block number for the chunk that was allocated;
; - size is the size chunk that is being taken or split
; or a sentinal value to indicate there was no eligible chunk
(define HBSC-1 (make-hbs-chunk 2 2))
(define (hbs-chunk-temp chunk)
  (...
   (cond [(boolean? chunk) ...]
         [(hbs-chunk? chunk) (... (hbs-chunk-block chunk) ...
                                  (hbs-chunk-size chunk) ...)])))



(define-struct hbs-alloc [block hbs])
; A HBSAllocResult is a (make-hbs-alloc Integer HBS)
; Represents the result of an allocation call, where:
; - block is the starting block number for the chunk that was allocated;
;   or -1 if no space available
; - hbs is the updated HBS after the allocation
(define HBSAR-1 (make-hbs-alloc 1
                                (make-hbs #f
                                          (make-hbs #f
                                                    (make-hbs #t #f #f)
                                                    (make-hbs #f #f #f))
                                          (make-hbs #t
                                                    (make-hbs #f #f #f)
                                                    (make-hbs #f #f #f)))))



;Exercise 1A

;Signature: blocks-remaining: HBS natNum -> Int

;Interpretation
; Will return the total number of free blocks within an hbs (those
; which represent true)

;tests

(check-expect (blocks-remaining
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #t #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))))
              3)

(check-expect (blocks-remaining
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))))
              4)

(check-expect (blocks-remaining
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))))
              6)

;code

(define (blocks-remaining hbs)
  (cond [(and (boolean? hbs) hbs) 1]
        [(and (boolean? hbs) (not hbs)) 0]
        [(hbs? hbs)
         (if (hbs-bit hbs)
             (chunk-size-cur? hbs)
             (+ (blocks-remaining (hbs-left hbs))
                (blocks-remaining (hbs-right hbs))))]))


;Signature: chunk-size-cur?: hbs -> int

;Interpretation
; Takes in an hbs and then returns the chunk size at the higest level of the hbs

;tests

(check-expect (chunk-size-cur?
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))))
              8)

(check-expect (chunk-size-cur?
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))))
              4)

(check-expect (chunk-size-cur?
               (make-hbs #t #f #f))
              1)

;code

(define (chunk-size-cur? hbs)
  (expt 2 (sub1 (hbs-length hbs))))
                                   


;Signature: hbs-length: hbs -> Int

;Interpretation:
; Will take in an hbs representing a binary tree, then
; returning the maximum depth of the tree

;tests

(check-expect (hbs-length
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))))
              4)

(check-expect (hbs-length
               (make-hbs #t #f #f))
              1)

(check-expect (hbs-length
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))))
              3)

;code

(define (hbs-length hbs)
  (cond [(boolean? hbs) 0]
        [(hbs? hbs)
         (+ 1
            (hbs-length (hbs-left hbs)))]))

;Exercise 1B

;Signature: find-chunk: hbs natNum -> int

;Interpretation
; Will return the index of the first free block of a given size
; if it is of size 2, will (if allowed) return a block of size 2 or
; will produce a larger block of size 4 if there are none of the
; requested size. -1 will be returned if there is no free space
; to fit this given block.

;tests

(check-expect (find-chunk
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #t #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f)))
               4)
              -1)

(check-expect (find-chunk
               (make-hbs #f #t #t)
               4)
              -1)

(check-expect (find-chunk
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f)))
               4)
              -1)

(check-expect (find-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f)))
               2)
              0)

(check-expect (find-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               2)
              4)

;code

(define (find-chunk hbs size)
  (if (boolean? (find-hbs-chunk hbs size))
      -1
      (hbs-chunk-block (find-hbs-chunk hbs size))))

;Signature: find-hbs-chunk: hbs natnum-> hbs-chunk

;Interpretation
; Takes in an hbs then returning the index of the value
; within the larger hbs as an hbs-chunk. Will return both
; the value of the index and size of the chunk that is going
; to be allocated. (size of the larger chunk, not the size that
; will be allocated)

;tests

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #f
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f)))
               2)
              (make-hbs-chunk 0 2))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #t #f #f)
                         (make-hbs #f #f #f))
               1)
              (make-hbs-chunk 0 1))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #f #f #f)
                         (make-hbs #t #f #f))
               1)
              (make-hbs-chunk 1 1))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #t #f #f)
                         (make-hbs #t #f #f))
               1)
              (make-hbs-chunk 0 1))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #f #f #f)
                         (make-hbs #f #f #f))
               1)
              #f)

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #f
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f)))
               1)
              (make-hbs-chunk 0 2))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #f
                                   (make-hbs #t #f #f)
                                   (make-hbs #f #f #f)))
               1)
              (make-hbs-chunk 2 1))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #f
                                   (make-hbs #t #f #f)
                                   (make-hbs #f #f #f)))
               1)
              (make-hbs-chunk 2 1))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #t #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               1)
              (make-hbs-chunk 2 1))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               2)
              (make-hbs-chunk 0 4))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f))
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f)))
                                   (make-hbs #f
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f))
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f))))
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f))
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f)))
                                   (make-hbs #f
                                             (make-hbs #f
                                                       (make-hbs #t #f #f)
                                                       (make-hbs #f #f #f))
                                             (make-hbs #f
                                                       (make-hbs #t #f #f)
                                                       (make-hbs #f #f #f)))))
               8)
              (make-hbs-chunk 0 8))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               2)
              (make-hbs-chunk 4 2))

(check-expect (find-hbs-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f))
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f)))
                                   (make-hbs #f
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f))
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f))))
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f))
                                             (make-hbs #f
                                                       (make-hbs #f #f #f)
                                                       (make-hbs #f #f #f)))
                                   (make-hbs #f
                                             (make-hbs #f
                                                       (make-hbs #t #f #f)
                                                       (make-hbs #f #f #f))
                                             (make-hbs #f
                                                       (make-hbs #t #f #f)
                                                       (make-hbs #f #f #f)))))
               1)
              (make-hbs-chunk 12 1))


;code

(define (find-hbs-chunk hbs size)
  (local [(define chunk-size (chunk-size-cur? hbs))
          (define (shift-chunk chunk)
            (if (boolean? chunk)
                chunk
                (make-hbs-chunk
                 (+ (hbs-chunk-block chunk)
                    (/ chunk-size 2))
                 (hbs-chunk-size chunk))))]
    (cond [(< size chunk-size)
           (local [(define chunk (smaller? (find-hbs-chunk (hbs-left hbs) size)
                                           (shift-chunk (find-hbs-chunk (hbs-right hbs) size))))]
             (if (boolean? chunk)
                 (hbs->chunk hbs chunk-size)
                 chunk))]
          [(= size chunk-size) (hbs->chunk hbs chunk-size)]
          [(> size chunk-size) #f])))
           

;Signature: hbs->chunk: hbs natNum -> hbs-chunk

;Interpretation:
; will take in an hbs and a natNum representing size
; and will check to see if it is
; true, if it is it will return an hbs, otherwise it will return
; false

;tests

(check-expect (hbs->chunk
               (make-hbs #t
                         (make-hbs #f #f #f)
                         (make-hbs #f #f #f))
               4)
              (make-hbs-chunk 0 4))

(check-expect (hbs->chunk
               (make-hbs #t
                         (make-hbs #f #f #f)
                         (make-hbs #f #f #f))
               2)
              (make-hbs-chunk 0 2))

(check-expect (hbs->chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #f
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f)))
               8)
              #false)

(check-expect (hbs->chunk
               (make-hbs #f #t #f)
               1)
              #f)

;code

(define (hbs->chunk hbs size)
  (if (hbs-bit hbs)
      (make-hbs-chunk 0 size)
      #false))

;Signature: smaller?: hbs-chunk hbs-chunk -> hbs-chunk

;Interpretation: will take in two hbs-chunks and then
; will return the hbs-chunk with the smaller size

;tests

(check-expect (smaller? (make-hbs-chunk 4 4)
                        (make-hbs-chunk 0 2))
              (make-hbs-chunk 0 2))

(check-expect (smaller? (make-hbs-chunk 0 2)
                        (make-hbs-chunk 0 1))
              (make-hbs-chunk 0 1))

(check-expect (smaller? (make-hbs-chunk 0 4)
                        (make-hbs-chunk 7 8))
              (make-hbs-chunk 0 4))

(check-expect (smaller? (make-hbs-chunk 0 4)
                        #f)
              (make-hbs-chunk 0 4))
                       
;code

(define (smaller? hbs1 hbs2)
  (cond
    [(and (boolean? hbs1) (boolean? hbs2)) #f]
    [(and (boolean? hbs1) (hbs-chunk? hbs2)) hbs2]
    [(and (hbs-chunk? hbs1) (boolean? hbs2)) hbs1]
    [(and (hbs-chunk? hbs1) (hbs-chunk? hbs2))
     (if (<= (hbs-chunk-size hbs1) (hbs-chunk-size hbs2))
         hbs1
         hbs2)]))

;Excersize 1C

;Signature: initialize-hbs: [List of Booleans] -> hbs

;Interpretation:
; Takes in a list of booleans that will have a multiple of 2
; inputs, then using that, will create a tree with said information

;tests

(check-expect (initialize-hbs
               (list #t #t #f #f))
              (make-hbs #f
                        (make-hbs #t
                                  (make-hbs #f #f #f)
                                  (make-hbs #f #f #f))
                        (make-hbs #f
                                  (make-hbs #f #f #f)
                                  (make-hbs #f #f #f))))

(check-expect (initialize-hbs
               (list #t #t #t #t #f #t #t #t))
              (make-hbs #f
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #t #f #f))
                                  (make-hbs #t
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (initialize-hbs
               (list #t #t #t #t))
              (make-hbs #t
                        (make-hbs #f
                                  (make-hbs #f #f #f)
                                  (make-hbs #f #f #f))
                        (make-hbs #f
                                  (make-hbs #f #f #f)
                                  (make-hbs #f #f #f))))

;code

(define (initialize-hbs lob)
  (if (= (length lob) 1)
      (make-hbs (first lob)
                #false
                #false)
      (if (andmap identity lob)
          (make-hbs #true
                    (initialize-hbs (build-list (/ (length lob) 2) boolean?))
                    (initialize-hbs (build-list (/ (length lob) 2) boolean?)))
          (make-hbs #false
                    (initialize-hbs (split-list-left lob))
                    (initialize-hbs (split-list-right lob))))))          

;lob with base case (lob)


;Signature: split-list-left: [List of Booleans] -> [List of Booleans] or -1

;Interpretation:
; Takes in a list of Booleans, then splitting it in half into two lists,
; returning the left split list.

;Tests

(check-expect (split-list-left (list #t #t #f #f #f #f #f #f))
              (list #t #t #f #f))

(check-expect (split-list-left (list #t #t #f #f))
              (list #t #t))

(check-expect (split-list-left (list #t #t))
              (list #t))

(check-expect (split-list-left (list #t))
              (list #t))

;code

(define (split-list-left lob)
  (local [(define len-lob (- (length lob) 1))
          (define (lob-recurser lobr len)
            (cond
              [(= len-lob 0) lobr]
              [(= (/ (- len-lob 1) 2) len) empty]
              [(cons? lob)
               (append (list (first lobr))
                       (lob-recurser (rest lobr) (- len 1)))]))]
    (lob-recurser lob len-lob)))


;Signature: split-list-right: [List of Booleans] -> [List of Booleans] or -1

;Interpretation:
; Takes in a list of Booleans, then splitting it in half into two lists,
; returning the right split list.

;Tests

(check-expect (split-list-right (list #t #t #f #f #f #f #f #f))
              (list #f #f #f #f))

(check-expect (split-list-right (list #t #t #f #f))
              (list #f #f))

(check-expect (split-list-right (list #t #t))
              (list #t))

(check-expect (split-list-right (list #t))
              (list #t))

;code

(define (split-list-right lob)
  (local [(define len-lob (- (length lob) 1))
          (define (lob-recurser len)
            (cond
              [(= len-lob 0) (list (get-element lob (floor (/ len-lob 2))))]
              [(= (/ (- len-lob 1) 2) len) empty]
              [(cons? lob)
               (append (lob-recurser (- len 1))
                       (list (get-element lob len)))]))]
    (lob-recurser len-lob)))


       
       
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
  

;Exercise 2

;Signature: alloc-chunk: hbs natNum ->hbsAlloc

;Interpretation:
; will take in an hbs and a size then it will
; change the hbs and give the index of allocation
; returning a hbs-alloc

;tests

(check-expect (alloc-chunk (make-hbs #false
                                     (make-hbs #false
                                               (make-hbs #true
                                                         (make-hbs #false
                                                                   (make-hbs #false #false #false)
                                                                   (make-hbs #false #false #false))
                                                         (make-hbs #false
                                                                   (make-hbs #false #false #false)
                                                                   (make-hbs #false #false #false)))
                                               (make-hbs #false
                                                         (make-hbs #false
                                                                   (make-hbs #false #false #false)
                                                                   (make-hbs #false #false #false))
                                                         (make-hbs #false
                                                                   (make-hbs #false #false #false)
                                                                   (make-hbs #true  #false #false))))
                                     (make-hbs #false
                                               (make-hbs #false
                                                         (make-hbs #false
                                                                   (make-hbs #false #false #false)
                                                                   (make-hbs #false #false #false))
                                                         (make-hbs #false
                                                                   (make-hbs #false #false #false)
                                                                   (make-hbs #false #false #false)))
                                               (make-hbs #false
                                                         (make-hbs #false
                                                                   (make-hbs #false #false #false)
                                                                   (make-hbs #false #false #false))
                                                         (make-hbs #false
                                                                   (make-hbs #false #false #false)
                                                                   (make-hbs #f #f #f))))) 1)
              (make-hbs-alloc 7
                              (make-hbs #false
                                        (make-hbs #false
                                                  (make-hbs #true
                                                            (make-hbs #false
                                                                      (make-hbs #f #f #f)
                                                                      (make-hbs #f #f #f))
                                                            (make-hbs #false
                                                                      (make-hbs #f #f #f)
                                                                      (make-hbs #f #f #f)))
                                                  (make-hbs #false
                                                            (make-hbs #false
                                                                      (make-hbs #f #f #f)
                                                                      (make-hbs #f #f #f))
                                                            (make-hbs #false
                                                                      (make-hbs #f #f #f)
                                                                      (make-hbs #f #f #f))))
                                        (make-hbs #false
                                                  (make-hbs #false
                                                            (make-hbs #false
                                                                      (make-hbs #f #f #f)
                                                                      (make-hbs #f #f #f))
                                                            (make-hbs #false
                                                                      (make-hbs #f #f #f)
                                                                      (make-hbs #f #f #f)))
                                                  (make-hbs #false
                                                            (make-hbs #false
                                                                      (make-hbs #f #f #f)
                                                                      (make-hbs #f #f #f))
                                                            (make-hbs #false
                                                                      (make-hbs #f #f #f)
                                                                      (make-hbs #f #f #f)))))))

(check-expect (alloc-chunk
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #t #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f)))
               4)
              (make-hbs-alloc -1
                              (make-hbs #f
                                        (make-hbs #f
                                                  (make-hbs #t #f #f)
                                                  (make-hbs #f #f #f))
                                        (make-hbs #t
                                                  (make-hbs #f #f #f)
                                                  (make-hbs #f #f #f)))))

(check-expect (alloc-chunk
               (make-hbs #f #t #t)
               4)
              (make-hbs-alloc -1
                              (make-hbs #f #t #t)))

(check-expect (alloc-chunk
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f)))
               4)
              (make-hbs-alloc -1
                              (make-hbs #f
                                        (make-hbs #f
                                                  (make-hbs #f #f #f)
                                                  (make-hbs #f #f #f))
                                        (make-hbs #t
                                                  (make-hbs #f #f #f)
                                                  (make-hbs #f #f #f)))))

(check-expect (alloc-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f)))
               2)
              (make-hbs-alloc 0
                              (make-hbs #f
                                        (make-hbs #f
                                                  (make-hbs #f #f #f)
                                                  (make-hbs #f #f #f))
                                        (make-hbs #t
                                                  (make-hbs #f #f #f)
                                                  (make-hbs #f #f #f)))))

(check-expect (alloc-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               2)
              (make-hbs-alloc 4
                              (make-hbs #f
                                        (make-hbs #t
                                                  (make-hbs #f
                                                            (make-hbs #f #f #f)
                                                            (make-hbs #f #f #f))
                                                  (make-hbs #f
                                                            (make-hbs #f #f #f)
                                                            (make-hbs #f #f #f)))
                                        (make-hbs #f
                                                  (make-hbs #f
                                                            (make-hbs #f #f #f)
                                                            (make-hbs #f #f #f))
                                                  (make-hbs #f
                                                            (make-hbs #f #f #f)
                                                            (make-hbs #f #f #f))))))

;code

(define (alloc-chunk hbs size)
  (if (= -1 (find-chunk hbs size))
      (make-hbs-alloc -1 hbs)
      (make-hbs-alloc
       (find-chunk hbs size)
       (change-value hbs (hbs-chunk-size (find-hbs-chunk hbs size))
                     (hbs-chunk-block (find-hbs-chunk hbs size)) size))))

;Signature: change-value: hbs natnum natnum natnum -> [List of Boolean]

;Interpretation:
; Takes in an hbs, a natnum representing the size of the allocation,
; and a natnum representing the index of the allocation as well as the
; size that is requested, then returns an
; hbs with the value changed and the list normalized

;tests

(check-expect (change-value
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               2 4 2)
              (make-hbs #f
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (change-value
               (make-hbs #t
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               8 0 2)
              (make-hbs #f
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #t
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (change-value
               (make-hbs #t
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               8 0 4)
              (make-hbs #f
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))
              
;code

(define (change-value hbs size-chunk index-chunk size-alloc)
  (local [(define current-size (chunk-size-cur? hbs))]
    (cond [(< size-chunk current-size)
           (if (>= index-chunk (/ current-size 2))
               (make-hbs (hbs-bit hbs) (hbs-left hbs) (change-value
                                                       (hbs-right hbs) size-chunk
                                                       (- index-chunk (/ current-size 2)) size-alloc))
               (make-hbs (hbs-bit hbs) (change-value (hbs-left hbs)
                                                     size-chunk index-chunk size-alloc)
                         (hbs-right hbs)))]
          [(>= size-chunk current-size)
           (if (= current-size size-alloc)
               (make-hbs #f
                         (change-value (hbs-left hbs)
                                       size-chunk index-chunk size-alloc)
                         (hbs-right hbs))
               (if (and (= index-chunk 0) (< size-alloc current-size))
                   (make-hbs #f
                             (change-value (set-hbs-true (hbs-left hbs))
                                           size-chunk index-chunk size-alloc)
                             (set-hbs-true (hbs-right hbs)))
                   (if (boolean? hbs)
                       hbs
                       (make-hbs (hbs-bit hbs)
                                 (hbs-left hbs)
                                 (hbs-right hbs)))))])))

;signature; set-hbs-true: hbs -> hbs

;Interpretation:
; Will take an hbs and then, utilizing the hbs will set the main value to true
; and will keep the rest the same.

;tests

(check-expect (set-hbs-true (make-hbs #f #f #f))
              (make-hbs #t #f #f))

(check-expect (set-hbs-true (make-hbs #t #f #f))
              (make-hbs #t #f #f))

(check-expect (set-hbs-true
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #f
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))))
              (make-hbs #t
                        (make-hbs #t
                                  (make-hbs #f #f #f)
                                  (make-hbs #f #f #f))
                        (make-hbs #f
                                  (make-hbs #f #f #f)
                                  (make-hbs #f #f #f))))
              
              
;code

(define (set-hbs-true hbs)
  (make-hbs #t
            (hbs-left hbs)
            (hbs-right hbs)))


;signature; set-hbs-false: hbs -> hbs

;Interpretation:
; Will take an hbs and then, utilizing the hbs will set the main value to false
; and will keep the rest the same.

;tests

(check-expect (set-hbs-false (make-hbs #f #f #f))
              (make-hbs #f #f #f))

(check-expect (set-hbs-false (make-hbs #t #f #f))
              (make-hbs #f #f #f))

(check-expect (set-hbs-false
               (make-hbs #t
                         (make-hbs #t
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))
                         (make-hbs #f
                                   (make-hbs #f #f #f)
                                   (make-hbs #f #f #f))))
              (make-hbs #f
                        (make-hbs #t
                                  (make-hbs #f #f #f)
                                  (make-hbs #f #f #f))
                        (make-hbs #f
                                  (make-hbs #f #f #f)
                                  (make-hbs #f #f #f))))
              
              
;code

(define (set-hbs-false hbs)
  (make-hbs #f
            (hbs-left hbs)
            (hbs-right hbs)))


              
;Exercise 3

;Signature: free-chunk: hbs Nat Int -> hbs

;Interpretation:
; will take in an hbs, a nat representing the size of the chunk
; that will be freed, and an int that represents the index
; that wants to be freed. Using this information, the hbs
; will be updated to change that value from false to true
; then making the value of true to false

;tests

(check-expect (free-chunk
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               2 0)
              (make-hbs #t
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (free-chunk
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #t #f #f))
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               1 0)
              (make-hbs #t
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (free-chunk
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               4 4)
              (make-hbs #t
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))



;code

(define (free-chunk hbs size index)
  (normalize-hbs (change-hbs-val hbs size index)))

;Siganture: change-hbs-val: hbs natnum Int -> hbs

;Interpretation:
; Takes in an hbs and then will, utilizing the size and index, find
; the given value and set it to true.

;tests

(check-expect (change-hbs-val
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               2 0)
              (make-hbs #f
                        (make-hbs #f
                                  (make-hbs #t
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #t
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (change-hbs-val
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               4 0)
              (make-hbs #f
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (change-hbs-val
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #t #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))))
               1 5)
              (make-hbs #f
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #t #f #f)
                                            (make-hbs #t #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))



(define (change-hbs-val hbs size index)
  (local [(define current-size (chunk-size-cur? hbs))]
    (cond [(< size current-size)
           (if (>= index (/ current-size 2))
               (make-hbs (hbs-bit hbs)
                         (hbs-left hbs)
                         (change-hbs-val (hbs-right hbs) size
                                         (- index (/ current-size 2))))
               (make-hbs (hbs-bit hbs)
                         (change-hbs-val (hbs-left hbs)
                                         size index)
                         (hbs-right hbs)))]
          [(= index 0)
           (set-hbs-true hbs)])))

;Signature: normalize-hbs: hbs -> hbs

;Interpretation:
; will take in a non normal hbs and then will return a normalalized hbs

;tests

(check-expect (normalize-hbs
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))))
              (make-hbs #t
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (normalize-hbs
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #t
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))))
              (make-hbs #f
                        (make-hbs #f
                                  (make-hbs #t
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (normalize-hbs
               (make-hbs #f
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #t #f #f)
                                             (make-hbs #t #f #f))
                                   (make-hbs #f
                                             (make-hbs #t #f #f)
                                             (make-hbs #t #f #f)))
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))))
              (make-hbs #t
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))

(check-expect (normalize-hbs
               (make-hbs #f
                         (make-hbs #t
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))
                         (make-hbs #f
                                   (make-hbs #f
                                             (make-hbs #t #f #f)
                                             (make-hbs #t #f #f))
                                   (make-hbs #f
                                             (make-hbs #f #f #f)
                                             (make-hbs #f #f #f)))))
              (make-hbs #f
                        (make-hbs #t
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))
                        (make-hbs #f
                                  (make-hbs #t
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f))
                                  (make-hbs #f
                                            (make-hbs #f #f #f)
                                            (make-hbs #f #f #f)))))


;code

(define (normalize-hbs hbs)
  (if (boolean? (hbs-left hbs))
      hbs
      (if (and (hbs-bit (normalize-hbs (hbs-left hbs)))
               (hbs-bit (normalize-hbs (hbs-right hbs))))
          (make-hbs #t
                    (set-hbs-false (normalize-hbs (hbs-left hbs)))
                    (set-hbs-false (normalize-hbs (hbs-right hbs))))
          (make-hbs (hbs-bit hbs)
                    (normalize-hbs (hbs-left hbs))
                    (normalize-hbs (hbs-right hbs))))))


