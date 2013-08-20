;;	SEARCH
;;	NOTE: Search is nondeterministic. Defined by Start and Goal
;;	states. The successor states are expanded in the search. 
;;	The order of exploration of states defines the type of
;;	search strategy being employed.
 
(defun tree-search (states goal successors combiner)
	"Basic Tree Search. No search strategy."
	(cond	((null states) nil)
			((funcall goal (first states)) (first states))
			(t (tree-search
				(funcall combiner
					(funcall successors (first states))
					(rest states))
				goal successors combiner))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prepend (x y) (append y x))
(defun is (value) #'(lambda (x) (eql x value)))

(defun diff (num) 
	"Abs of the difference."
	#'(lambda (x) (abs (- x num))))

(defun sorter (cost-fn)
	"Combiner that sorts based on cost function"
	#'(lambda (new old)
		(sort (append new old) #'< :key cost-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun depth-first-search (start goal successors)
 	"Uses Depth First search strategy."
	(tree-search (list start) goal successors #'append))

(defun breadth-first-search (start goal successors)
	(tree-search (list start) goal successors #'prepend))

(defun best-first-search (start goal successors cost-fn)
	(tree-search (list start) goal successors (sorter cost-fn)))

(defun beam-search (start goal successors cost-fn beam-width)
	(tree-search (list start) goal successors
		#'(lambda (old new)
			(let ((sorted (funcall (sorter cost-fn) old new)))
				(if (> beam-width (length sorted))
					sorted
					(subseq sorted 0 beam-width))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;; Testing Uninformed search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))
(depth-first-search 6 (is 12) #'binary-tree)
(breadth-first-search 1 (is 12) #'binary-tree)

(defun finite-binary-tree (n)
	#'(lambda (x)
		(remove-if #'(lambda (child) (> child n))
			(binary-tree x))))

(depth-first-search 1 (is 12) (finite-binary-tree 15))
(breadth-first-search 1 (is 12) (finite-binary-tree 15))

(best-first-search 1 (is 12) #'binary-tree (diff 12))
(beam-search 1 (is 4) #'binary-tree (diff 4) 2)
