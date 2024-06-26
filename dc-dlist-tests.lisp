;; Run these tests with
;;   (prove:run #P"/home/macnod/common-lisp/dc-dlist/dc-dlist-tests.lisp")

(in-package :cl-user)
(require :dc-dlist)
(require :prove)
(defpackage :dc-dlist-tests (:use :cl :prove))
(in-package :dc-dlist-tests)

(plan 51)

(let ((dlist (dc-dlist::from-list '(1 2 3))))
  (is (dc-dlist::to-list dlist) '(1 2 3))
  (is (dc-dlist::at dlist 1) 2 "at 1 of '(1 2 3)) is 2")
  (is (dc-dlist::at dlist 0) 1 "at 0 of '(1 2 3)) is 1")
  (is (dc-dlist::at dlist 2) 3 "at 2 of '(1 2 3)) is 3")
  (is (dc-dlist::at dlist -1) nil "at -1 of '(1 2 3)) is nil")
  (is (dc-dlist::at dlist 3) nil "at 3 of '(1 2 3)) is nil")
  (dc-dlist::push-head dlist 0)
  (is (dc-dlist::to-list dlist) '(0 1 2 3)
      "push-head '(1 2 3) 0 -> '(0 1 2 3)")
  (dc-dlist::push-tail dlist 4)
  (is (dc-dlist::to-list dlist) '(0 1 2 3 4)
      "push-tail '(0 1 2 3) 4 -> '(0 1 2 3 4)")
  (is (dc-dlist::peek-head dlist) 0
      "peek-head '(0 1 2 3 4) -> 0)")
  (is (dc-dlist::pop-head dlist) 0
      "pop-head '(0 1 2 3 4) -> 0, and list diminished")
  (is (dc-dlist::peek-head dlist) 1 "peek-head '(1 2 3 4) -> 1")
  (is (dc-dlist::to-list dlist) '(1 2 3 4) "list is '(1 2 3 4)")
  (is (dc-dlist::peek-tail dlist) 4 "peek-tail is 4")
  (is (dc-dlist::pop-tail dlist) 4 "pop-tail is 4")
  (is (dc-dlist::to-list dlist) '(1 2 3) "list is '(1 2 3)")
  (is (dc-dlist::pop-tail dlist) 3 "pop-tail is 3")
  (is (dc-dlist::pop-tail dlist) 2 "pop-tail is 2")
  (is (dc-dlist::pop-tail dlist) 1 "pop-tail is 1")
  (is (dc-dlist::to-list dlist) nil "dlist is nil after last pop")
  (dc-dlist::push-tail dlist 1)
  (is (dc-dlist::pop-head dlist) 1 "pop-head is 1 after (push-tail dlist 1)")
  (is (dc-dlist::to-list dlist) nil "dlist is nil after last pop-head")
  (is (dc-dlist::len dlist) 0 "len dlist is 0 after last pop-head")
  (is (dc-dlist::pop-tail dlist) nil "pop-tail is nil when dlist is empty")
  (is (dc-dlist::pop-head dlist) nil "pop-head is nil when dlist is empty")
  (dc-dlist::push-tail dlist 1)
  (dc-dlist::push-tail dlist 2)
  (dc-dlist::push-tail dlist 3)
  (is (dc-dlist::peek-tail dlist) 3 "peek-tail now 3")
  (let ((even-value-node (dc-dlist::find-first-node dlist #'evenp)))
    (is (dc-dlist::value even-value-node) 2 "first even value is 2")
    (ok (dc-dlist::contains-node dlist even-value-node)
        "dlist contains given node")
    (dc-dlist::insert-before-node dlist even-value-node 1.5)
    (is (dc-dlist::to-list dlist) '(1 1.5 2 3)
        "insertion of 1.5 near midpoint succeeds")
    (dc-dlist::insert-after-node dlist even-value-node 2.5)
    (is (dc-dlist::to-list dlist) '(1 1.5 2 2.5 3))
    (is (dc-dlist::at dlist 1) 1.5 "second element is now 1.5")
    (dc-dlist::delete-node dlist even-value-node)
    (is (dc-dlist::to-list dlist) '(1 1.5 2.5 3)
        "list looks right after deleting the node with the even value")
    (ok (not (dc-dlist::delete-node dlist (make-instance 'dc-dlist::dlist-node
                                                         :prev nil
                                                         :next nil
                                                         :value 9)))
        "deleting a non-existing node from a dlist returns nil")))

(ok (not (let ((dlist (dc-dlist::from-list '(1 2 3))))
           (dc-dlist::delete-node-at dlist 5)))
    "delete-node-at returns nil when it sees an index that is out of range.")

(ok (loop with dlist = (make-instance 'dc-dlist::dlist)
          for a from 1 to 100 do (dc-dlist::push-tail dlist a)
          always (and (= (dc-dlist::len dlist) a)
                      (= (dc-dlist::peek-head dlist) 1)
                      (= (dc-dlist::peek-tail dlist) a)))
    "length of list adjusts correctly with push-tail")

(ok (loop with dlist = (make-instance 'dc-dlist::dlist)
          for a from 1 to 100 do (dc-dlist::push-head dlist a)
          always (and (= (dc-dlist::len dlist) a)
                      (= (dc-dlist::peek-head dlist) a)
                      (= (dc-dlist::peek-tail dlist) 1)))
    "length of list adjusts correctly with push-head")

(ok (let ((dlist (dc-dlist::from-list '(1 2 3))))
      (dc-dlist::delete-node-at dlist 1)
      (loop for node = (dc-dlist::head dlist) then (dc-dlist::next node)
            while node
            always (oddp (dc-dlist::value node))))
    "index-based node deletion works")

(ok (let* ((list (loop for a from 1 to 100 collect a))
           (dlist (dc-dlist::from-list list)))
      (loop for node = (dc-dlist::find-first-node dlist #'oddp)
            while node do (dc-dlist::delete-node dlist node))
      (loop for value in (dc-dlist::to-list dlist)
            always (evenp value)))
    "node-based node deletion works")

(let ((dlist (dc-dlist::from-list '(1 2 3))))
  (ok (and (null (dc-dlist::delete-node-at dlist -1))
           (null (dc-dlist::delete-node-at dlist 3)))
      "deleting nodes by index using out-of-bound index returns null")
  (is (dc-dlist::delete-node-at dlist 0) 1
      "deleting first node, with value 1, returns 1")
  (is (dc-dlist::delete-node-at dlist 0) 2
      "deleting first node, now with value 2, returns 2")
  (is (dc-dlist::delete-node-at dlist 0) 3
      "deleting first node, now with value 3, returns 3")
  (is (dc-dlist::delete-node-at dlist 0) nil
      "deleting first node in a list with no nodes returns nil")
  (dc-dlist::push-tail dlist 2)
  (dc-dlist::push-tail dlist 3)
  (dc-dlist::push-head dlist 1)
  (ok (loop for a from 1 to 3
            for node = (dc-dlist::head dlist) then (dc-dlist::next node)
            always (= (dc-dlist::value node) a))
      "adding values to a list that was previously emptied out works."))
(let* ((dlist (dc-dlist::from-list '(1 2 3)))
       (dlist-copy (dc-dlist::copy dlist)))
  (is (dc-dlist::len dlist) (dc-dlist::len dlist-copy)
      "copy produces dlist with same number of nodes as original")
  (is (dc-dlist::to-list dlist) (dc-dlist::to-list dlist-copy)
      "elements in copy are the same as elements in original")
  (dc-dlist::pop-head dlist)
  (isnt (dc-dlist::to-list dlist)
        (dc-dlist::to-list dlist-copy)
        "after popping original, elements not the same as elements in copy"))

(let* ((list '(3 2 4 1 5))
       (sorted-list (sort (copy-list list) #'<))
       (dlist (dc-dlist::from-list list))
       (sorted-dlist (dc-dlist::sorted dlist #'<)))
  (is sorted-list (dc-dlist::to-list sorted-dlist) "sorting works."))

(loop
  with list = '(0 1 2 3)
  with dlist = (dc-dlist::from-list list)
  for value in list
  for node = (dc-dlist::head dlist) then (dc-dlist::next node)
  for node-index = (dc-dlist::node-index node)
  do (is node-index value
         (if (< value 3)
             (format nil "node-index returns ~d for the ~:r node"
                     node-index (1+ value))
             (format nil "node-index returns ~d for the last node"
                     node-index))))

(finalize)
