#lang racket
(require csc151)







;_________________________________________Godly Whitelist______________________________________________________

(define Whitelist (file->words "/home/thomasbr/Desktop/Data/Whitelist.txt"))

;________________________________Procedures for common words_______________________________________________

;;; Procedure:
;;;   gods-own-downcase
;;; Parameters:
;;;   lst, a list of chars
;;; Purpose:
;;;   converts all the letters in lst to lowercase
;;; Produces:
;;;   result, a list of chars

(define gods-own-downcase
  (lambda (lst)
    (let kernel ([newlist null]
                 [leest lst])
      (cond
        [(null? leest)
         newlist]
        [else
         (kernel (append newlist (list
                                  (list->string (map char-downcase
                                                     (string->list (car leest))))))
                 (cdr leest))]))))

;;; Procedure:
;;;   top5
;;; Parameters:
;;;   fname, a txt file call
;;; Purpose:
;;;   Display the top 5 words used, along with their frequency
;;; Produces:
;;;   top5, a list of lists, with each inner list having the common word, and then how many instances of the word there are in the file.
;;; Preconditions:
;;;   file must be a txt file
;;; Postconditions:
;;;   word is absent from the whitelist


(define top5
  (lambda (fname)
    (let* ([words1 (gods-own-downcase (file->words fname))]
           [words (filter (lambda (x)
                            (= -1 (index-of x Whitelist)))words1 )]
           [tallied (tally-all words)]
           [num-words (length words)]
           [x (lambda (a b)
                (< (cadr a) (cadr b)))]
           [lst (sort tallied x)])
      (take (reverse lst) 5))))

;All top 5 words used by the 11 presidents.
;Code used for Compare5 procedure. 
(define all-spch (append (top5 "/home/thomasbr/Desktop/Data/FDR.txt")
                         (top5 "/home/thomasbr/Desktop/Data/JFK.txt")
                         (top5 "/home/thomasbr/Desktop/Data/Lincoln.txt")
                         (top5 "/home/thomasbr/Desktop/Data/Nixon.txt")
                         (top5 "/home/thomasbr/Desktop/Data/Obama.txt")
                         (top5 "/home/thomasbr/Desktop/Data/Reagan.txt")
                         (top5 "/home/thomasbr/Desktop/Data/TeddyR.txt")
                         (top5 "/home/thomasbr/Desktop/Data/TJeff.txt")
                         (top5 "/home/thomasbr/Desktop/Data/Trump.txt")
                         (top5 "/home/thomasbr/Desktop/Data/Washington.txt")
                         (top5 "/home/thomasbr/Desktop/Data/Wilson.txt")))


;___________________________Procedures for compare5_______________________________________

;;; Procedure:
;;;   remove-at
;;; Parameters:
;;;   pos, an index within the lst
;;;   lst, a list 
;;; Purpose:
;;;   creates a new veresion of lst with the value at position pos removed.
;;; Produces:
;;;   result, a list of elements with the value at the given pos removed
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   length of output list must be 1 less than length of original lst 
(define remove-at
  (lambda (pos lst)
    (help-remove pos lst 0)))


;;; Procedure:
;;;   help-remove-at
;;; Parameters:
;;;   pos, an index within the lst
;;;   lst, a list
;;;   count, a number 
;;; Purpose:
;;;   helps execute remove-at
;;; Produces:
;;;   result, a list


(define help-remove
  (lambda (pos lst count)
    (let ([newlin null]
          [last (length lst)])
      (cond
        [(null? lst)
         (append null lst)]
        [(= count pos)
         (append null (cdr lst))]
        [else
         (cons (car lst) (help-remove pos (cdr lst) (+ 1 count)))]))))

;;; Procedure:
;;;   sortr
;;; Parameters:
;;;   lst, a list 
;;; Purpose:
;;;   sorts the list by second element
;;; Produces:
;;;   result, a list


(define sortr
  (lambda (lst)
    (let* ([x (lambda (a b)
                (< (cadr a) (cadr b)))]
           [lost (sort lst x)])
      (take (reverse lost) 5))))


;;; Procedure:
;;;   compare5
;;; Parameters:
;;;   lst, a list of txt files
;;; Purpose:
;;;   Prints the 5 most common words from a list of files along with their frequency
;;; Produces:
;;;  results, a list of lists
;;; Preconditions:
;;;   lst must have valid entries
;;; Postconditions:
;;;   results will only contain words that are present in lst
;;;   if 2 words have the same frequency, then the word that appears before in lst is used in results

(define compare5
  (lambda (lst)
    (let ([mylst (map car lst)])
      (let kernel ([nlst  mylst]
                   [newlst null])
        (cond
          [(null? nlst)
           (sortr (tally-all newlst))]
          [else
           (if (= -1 (index-of (car nlst)
                               (remove-at (index-of (car nlst) mylst) mylst)))
               (kernel (cdr nlst) newlst)
               (kernel (cdr nlst) (cons (car nlst) newlst)))])))))

;_____________________________Word Association_______________________________________


;;; Procedure:
;;;   common-connections-helper
;;; Parameters:
;;;   lst1, a list
;;;   index, a number
;;; Purpose:
;;;   makes pairs of strings from lst
;;; Produces:
;;;  results, a list of lists

      
(define common-connections-helper
  (lambda (lst1 index)
    (if (= (+ 1 index) (length lst1))
        '()
        (append (list (list
                       (list-ref lst1 index)
                       (list-ref lst1 (+ 1 index))))
                (common-connections-helper lst1 (+ index 1))))))

;;; Procedure:
;;;   common-connections
;;; Parameters:
;;;  fname, a txt file
;;; Purpose:
;;;   gives the top 5 pairs of words with the highest frequency
;;; Produces:
;;;  results, a list of lists
;;; Preconditions:
;;;  fname must be a valid path
;;; Postconditions:
;;;  words do not appear in whitelist
;;;  if two pairs have the same frequency, the one that appears first will be in results


(define common-connections
  (lambda (fname)
    (let* ([words1 (gods-own-downcase (file->words fname))]
           [words (filter (lambda (x)
                            (= -1 (index-of x Whitelist))) words1 )]
           [lst1 (tally-all(common-connections-helper words 0))]
           [x (lambda (a b)
                (> (cadr a) (cadr b)))]
           [lst (sort lst1 x)])
      (let ([newlst
             (filter (lambda (x)
                       (not (= 1 (cadr x)))) lst) ])       
        (cond
          [(< (length newlst) 5)
           newlst]
          [else
           (take newlst 5)])))))
     


;________________________________Last Sentence____________________________________________

;;; Procedure:
;;;   last-sentence
;;; Parameters:
;;;  fil, a txt file
;;; Purpose:
;;;   gives the last sentence of fil
;;; Produces:
;;;  results, a string
;;; Preconditions:
;;;  fname must be a valid path
;;;  must follow grammar rules i.e must include periods (.) 
;;; Postconditions:
;;;  the last sentence is reproduced verbatim

(define last-sentence
  (lambda (fil)
    (let ([newlst (filter (lambda (x)
                            (not (equal? #\newline x)))
                          (reverse (file->chars fil)))])
      (list->string (filter (lambda (x)
                              (or (char-alphabetic? x)
                                  (equal? #\. x)
                                  (equal? #\, x)
                                  (char-whitespace? x)))
                            (reverse (take newlst
                                           (+ 1 (index-of #\.
                                                          (drop newlst
                                                                (+ 1 (index-of #\. newlst))))))))))))

;All last sentences together      

(define all-last-sentence
  (append (list "Washington")
          (list (last-sentence "/home/thomasbr/Desktop/Data/Washington.txt"))
          (list "Thomas Jefferson")
          (list (last-sentence "/home/thomasbr/Desktop/Data/TJeff.txt"))
          (list "")
          (list "Lincoln")
          (list (last-sentence "/home/thomasbr/Desktop/Data/Lincoln.txt"))
          (list "")
          (list "Teddy Roosevelt")
          (list (last-sentence "/home/thomasbr/Desktop/Data/TeddyR.txt"))
          (list "")
          (list "Wilson")
          (list (last-sentence "/home/thomasbr/Desktop/Data/Wilson.txt"))
          (list "")
          (list "FDR")
          (list (last-sentence "/home/thomasbr/Desktop/Data/FDR.txt"))
          (list "")
          (list "JFK")
          (list (last-sentence "/home/thomasbr/Desktop/Data/JFK.txt"))
          (list "")
          (list "Nixon")
          (list (last-sentence "/home/thomasbr/Desktop/Data/Nixon.txt"))
          (list "")
          (list "Reagan")
          (list (last-sentence "/home/thomasbr/Desktop/Data/Reagan.txt"))
          (list "")
          (list "Obama")
          (list (last-sentence "/home/thomasbr/Desktop/Data/Obama.txt"))
          (list "")
          (list "Trump")
          (list (last-sentence "/home/thomasbr/Desktop/Data/Trump.txt"))))



;_______________________________Final Code________________________________________________


(define final-code
  (lambda (magic-code)
    (if (eq? magic-code "Nice Computer")
        (and (display "Total")
             (newline)
             (display (top5 "/home/thomasbr/Desktop/Data/Dataset.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together across all Presidents")
             (newline)
             (display (common-connections "/home/thomasbr/Desktop/Data/Dataset.txt"))
             (newline)
             (newline)
             (newline)
             (display "Washington")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/Washington.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/Washington.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/Washington.txt"))
             (newline)
             (newline)
             (newline)
             (display "Thomas Jefferson")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/TJeff.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/TJeff.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/TJeff.txt"))
             (newline)
             (newline)
             (newline)
             (display "Lincoln")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/Lincoln.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/Lincoln.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/Lincoln.txt"))
             (newline)
             (newline)
             (newline)
             (display "TeddyR")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/TeddyR.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/TeddyR.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/TeddyR.txt"))
             (newline)
             (newline)
             (newline)
             (display "Wilson")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/Wilson.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/Wilson.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/Wilson.txt"))
             (newline)
             (newline)
             (newline)
             (display "FDR")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/FDR.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/FDR.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/FDR.txt"))
             (newline)
             (newline)
             (newline)
             (display "JFK")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/JFK.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/JFK.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/JFK.txt"))
             (newline)
             (newline)
             (newline)
             (display "Nixon")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/Nixon.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/Nixon.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/Nixon.txt"))
             (newline)
             (newline)
             (newline)
             (display "Reagan")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/Reagan.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/Reagan.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/Reagan.txt"))
             (newline)
             (newline)
             (newline)
             (display "Obama")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/Obama.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/Obama.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/Obama.txt"))
             (newline)
             (newline)
             (newline)
             (display "Trump")
             (newline)
             (display "Top Five words used in speech")
             (newline)
             (display (top5  "/home/thomasbr/Desktop/Data/Trump.txt"))
             (newline)
             (newline)
             (display "2 words commonly found together")
             (newline)
             (display (common-connections  "/home/thomasbr/Desktop/Data/Trump.txt"))
             (newline)
             (newline)
             (display "Last Sentence from the speech")
             (newline)
             (display (last-sentence "/home/thomasbr/Desktop/Data/Trump.txt")))
        "Use the magic command")))
