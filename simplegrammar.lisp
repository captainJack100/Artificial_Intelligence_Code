;;
;;	Basic grammar
;;
;;	S -> NP + VP
;;	NP -> Art + N
;;	VP -> V + NP
;;	Art -> THE, A, ...
;;	N -> MAN, BALL, WOMAN, TABLE, ...
;;	V -> HIT, TOOK, SAW, LIKED, ...
;;
;;	Generate a sentence --->
;;		(generate 'sentence)
;;

(defparameter *big-grammar*
	'((sentence -> (noun-phrase verb-phrase))
	  (noun-phrase -> (article adj* noun pp*) (name) (pronoun))
	  (verb-phrase -> (verb noun-phrase pp*))
	  (pp* -> () (pp pp*))
	  (adj* -> () (adj adj*))
	  (pp -> (prep noun-phrase))
	  (prep -> TO IN BY WITH ON)
	  (adj -> BIG LITTLE BLUE GREEN ADIABATIC)
	  (article -> THE A)
	  (name -> MAN BALL WOMAN TABLE)
	  (verb -> HIT TOOK SAW LIKED)
	  (pronoun -> HE SHE IT THESE THOSE THAT)))	

(defparameter *simple-grammar*
	'((sentence -> (noun-phrase verb-phrase))
	  (noun-phrase -> (Article Noun))
	  (verb-phrase -> (Verb noun-phrase))
	  (Article -> THE A)
	  (Noun -> MAN BALL WOMAN TABLE)
	  (VERB -> HIT TOOK SAW LIKED))
	  "Simple grammar.")

(defvar *grammar* *simple-grammar*
	"The grammar used to generate")

(defun rule-lhs (rule)
	"The left hand side of a rule"
	(first rule))

(defun rule-rhs (rule)
	"The right hand side of a rule"
	(rest (rest rule)))

(defun rewrites (category)
	"Return a list of the possible rewrites for this category. Assoc takes a list of
	lists and returns the first instance of the key."
	(rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
	"Generate a random sentence or phrase
	1. Check if list if it is apply function to list
	2. Pick random elt from list
	3. if it's a word return it as a list."
	(cond	((listp phrase) (mappend #'generate phrase))
			((rewrites phrase) (generate (random-elt (rewrites phrase))))
			(t (list phrase))))

(defun generate-parse-tree (phrase)
	"Generate a parse tree"
	(cond	((listp phrase) (mapcar #'generate-parse-tree phrase))
			((rewrites phrase) (cons phrase (generate-parse-tree (random-elt (rewrites phrase)))))
			(t (list phrase))))
			
(defun mappend (fn the-list)
	"Apply a fn to each elt of list and append results"
	(apply #'append (mapcar fn the-list)))

(defun random-elt (choices)	
	(elt choices (random (length choices))))
