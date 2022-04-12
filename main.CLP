;----- Author: Filotas Sismanis

(defclass house "house class that contains the needed slots for the houses in the database"
	(is-a USER)
    (slot size (type NUMBER))
	(multislot area (type SYMBOL) (allowed-symbols east west centre))
	(slot location (type SYMBOL))
	(slot age (type SYMBOL) (allowed-symbols New Old))
    (slot type (type SYMBOL) (allowed-symbols Apartment Detached))
    (slot rooms (type NUMBER))
	(slot parking (type SYMBOL) (allowed-symbols yes no))
	(slot price (type NUMBER))
	(slot candidate (type SYMBOL) (allowed-symbols yes no) (default yes))
)

(definstances house-database "house-database instances of all the available houses with their respective characteristics and information"
	(spiti-no-1 of house (size 95)(area east)(location Kalamaria)(age Old)(type Apartment)(rooms 3)(parking no)(price 190000))
	(spiti-no-2 of house (size 105)(area east)(location Pylaia)(age Old)(type Detached)(rooms 2)(parking yes)(price 189000))
	(spiti-no-3 of house (size 111)(area east)(location Toumpa)(age New)(type Apartment)(rooms 3)(parking yes)(price 177600))
	(spiti-no-4 of house (size 84)(area centre)(location AnwPolh)(age Old)(type Apartment)(rooms 2)(parking no)(price 142800))
	(spiti-no-5 of house (size 97)(area centre)(location AgiosPaulos)(age New)(type Detached)(rooms 2)(parking no)(price 145500))
	(spiti-no-6 of house (size 93)(area centre)(location Sykies)(age New)(type Apartment)(rooms 2)(parking no)(price 120900))
	(spiti-no-7 of house (size 120)(area west)(location Stauroupolh)(age New)(type Detached)(rooms 3)(parking no)(price 144000))
	(spiti-no-8 of house (size 130)(area west)(location Euosmos)(age New)(type Apartment)(rooms 4)(parking yes)(price 130000))
	(spiti-no-9 of house (size 92)(area west)(location Menemenh)(age Old)(type Apartment)(rooms 2)(parking no)(price 128800))
	(spiti-no-10 of house (size 115)(area east)(location Pylaia)(age New)(type Apartment)(rooms 3)(parking yes)(price 210000))
)

(deffunction ask-question (?question $?allowed-values) "function that is used in order to ask the user a question and check if the input is one of the allowed values"
	(printout t ?question)
	(bind ?answer (read))
	(if (lexemep ?answer) 
		then (bind ?answer (lowcase ?answer)))
	(while (not (member ?answer ?allowed-values)) do
		(printout t ?question)
		(bind ?answer (read))
		(if (lexemep ?answer) 
			then (bind ?answer (lowcase ?answer))))
	?answer)

(deffunction ask-number (?question) "function that reads a number input"
	(printout t ?question)
	(bind ?answer (read))
	(return ?answer)
)

;--------------------

(defrule init "the initial rule that asks the user for their desired house and saves their answers to be checked later on"
	(initial-fact)
	=>
	(bind ?usersize  (ask-question "Do you want a small, medium or large house? " small medium large))
	(assert (usersize ?usersize))
	
	(bind ?userarea  (ask-question "Which area (east/centre/west)? " east centre west))
	(assert (userarea ?userarea)) 
	
	(bind ?new  (ask-question "Do you want a new house (yes/no)? " yes no))
	(if (eq ?new yes) 
		then 
			(assert (userage New)) 
		else 
			(if (eq ?new no)
				then
				(assert (userage Old))
			)
	)
	
	(bind ?t (ask-question "Apartment or detached (a/d)? " a d))
	(if (eq ?t a) 
		then 
			(assert (usertype Apartment)) 
		else 
			(if (eq ?t d)
				then
				(assert (usertype Detached))
			)
	)
	
	(bind ?family  (ask-number "How many family members? "))
	(assert (family ?family))
	
	(bind ?car  (ask-question "Do you have a car (yes/no)? " yes no))
	(assert (car ?car))
	
	(printout t crlf)
)

(defrule small-size "rule that determines the maximum size a house can be, in case the user wants a small one"
	(usersize ?x)
	(test (eq ?x small))
	(family ?y)
	=>
	(bind ?maxsize (* ?y 25))
	(assert (maxsize ?maxsize))
)

(defrule medium-size "rule that determines the minimum and maximum size a house can be, in case the user wants a medium one"
	(usersize ?x)
	(test (eq ?x medium))
	(family ?y)
	=>
	(bind ?maxsize (* ?y 35))
	(assert (maxsize ?maxsize))
	(bind ?minsize (* ?y 25))
	(assert (minsize ?minsize))
)

(defrule big-size "rule that determines the minimum size a house can be, in case the user wants a big one"
	(usersize ?x)
	(test (eq ?x large))
	(family ?y)
	=>
	(bind ?minsize (* ?y 35))
	(assert (minsize ?minsize))
)

(defrule rooms "rule that determines the minimum and maximum amount of rooms a house can have, based on the user's family size"
	(family ?x)
	=>
	(bind ?minrooms (/ ?x 2))
	(assert (minrooms ?minrooms))
	(bind ?maxrooms (- ?x 1))
	(assert (maxrooms ?maxrooms))
)

(defrule check-small-size "rule to check if the house has a small size"
	(object (is-a house) (name ?n) (candidate yes) (size ?s))
	(usersize ?u)
	(maxsize ?m)
	(test (eq ?u small))
	(test (not (< ?s ?m)))
  =>
  	(modify-instance ?n (candidate no))
)

(defrule check-medium-size "rule to check if the house has a medium size"
	(object (is-a house) (name ?n) (candidate yes) (size ?s))
	(usersize ?u)
	(minsize ?min)
	(maxsize ?max)
	(test (eq ?u medium))
	(test (not (and (<= ?s ?max) (>= ?s ?min))))
  =>
  	(modify-instance ?n (candidate no))
)

(defrule check-big-size "rule to check if the house has a big size"
	(object (is-a house) (name ?n) (candidate yes) (size ?s))
	(usersize ?u)
	(minsize ?m)
	(test (eq ?u large))
	(test (not (> ?s ?m)))
  =>
  	(modify-instance ?n (candidate no))
)

(defrule check-area "rule to check the area of the house"
	(object (is-a house) (name ?n) (candidate yes) (area ?a))
	(userarea ?u)
	(test (not (eq ?a ?u)))
  =>
  	(modify-instance ?n (candidate no))
)


(defrule check-age "rule to check how old the house is"
	(object (is-a house) (name ?n) (candidate yes) (age ?a))
	(userage ?u)
	(test (not (eq ?a ?u)))
  =>
  	(modify-instance ?n (candidate no))
)


(defrule check-type "rule to check the type of the house"
	(object (is-a house) (name ?n) (candidate yes) (type ?t))
	(usertype ?u)
	(test (not (eq ?t ?u)))
  =>
  	(modify-instance ?n (candidate no))
)

(defrule check-rooms "rule to check if the amount of rooms is suitable for the user"
	(object (is-a house) (name ?n) (candidate yes) (rooms ?r))
	(minrooms ?min)
	(maxrooms ?max)
	(test (not (and (>= ?r ?min) (<= ?r ?max))))
  =>
  	(modify-instance ?n (candidate no))
)

(defrule check-parking "rule to check if the house has or doesn't have a parking"
	(object (is-a house) (name ?n) (candidate yes) (parking ?p))
	(car ?c)
	(test (not (eq ?p ?c)))
  =>
  	(modify-instance ?n (candidate no))
)

(defrule print-result "a rule to print out the results"
   (declare (salience -10))
   (object (is-a house) (name ?x) (candidate yes) (size ?s) (rooms ?r) (age ?a) (type ?t) (parking ?p) (location ?l) (price ?pr))
   =>
	(printout t "A suitable house for you is " (instance-name-to-symbol ?x) crlf)
	(printout t "    Size: " ?s crlf)
	(printout t "    Rooms: " ?r crlf)
	(printout t "    Age: " ?a crlf)
	(printout t "    Type: " ?t crlf)
	(printout t "    Parking: " ?p crlf)
	(printout t "    Area: " ?l crlf)
	(printout t "    Price: " ?pr crlf)
	(printout t crlf)
)

(defrule print-no-result "a rule to print out if there are no results"
   (declare (salience -15))
   (not(object (is-a house) (name ?x) (candidate yes)))
   =>
	(printout t "There is no house based on your criteria." crlf)
)