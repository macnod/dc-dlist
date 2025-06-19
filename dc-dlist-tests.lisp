;; Run these tests with
;;   (prove:run #P"/home/macnod/common-lisp/dc-dlist/dc-dlist-tests.lisp")
;; Or, from the shell, with
;;   make test

(in-package :cl-user)

(pushnew (truename ".") asdf:*central-registry* :test 'equal)
(asdf:load-system :dc-dlist)

(require :prove)
(defpackage :dc-dlist-tests 
  (:use :cl :prove)
  (:local-nicknames (:dl :dc-dlist)))
(in-package :dc-dlist-tests)

(plan 51)

(let ((dlist (dl:from-list '(1 2 3))))
  (is (dl:to-list dlist) '(1 2 3))
  (is (dl:at dlist 1) 2 "at 1 of '(1 2 3)) is 2")
  (is (dl:at dlist 0) 1 "at 0 of '(1 2 3)) is 1")
  (is (dl:at dlist 2) 3 "at 2 of '(1 2 3)) is 3")
  (is (dl:at dlist -1) nil "at -1 of '(1 2 3)) is nil")
  (is (dl:at dlist 3) nil "at 3 of '(1 2 3)) is nil")
  (dl:push-head dlist 0)
  (is (dl:to-list dlist) '(0 1 2 3)
      "push-head '(1 2 3) 0 -> '(0 1 2 3)")
  (dl:push-tail dlist 4)
  (is (dl:to-list dlist) '(0 1 2 3 4)
      "push-tail '(0 1 2 3) 4 -> '(0 1 2 3 4)")
  (is (dl:peek-head dlist) 0
      "peek-head '(0 1 2 3 4) -> 0)")
  (is (dl:pop-head dlist) 0
      "pop-head '(0 1 2 3 4) -> 0, and list diminished")
  (is (dl:peek-head dlist) 1 "peek-head '(1 2 3 4) -> 1")
  (is (dl:to-list dlist) '(1 2 3 4) "list is '(1 2 3 4)")
  (is (dl:peek-tail dlist) 4 "peek-tail is 4")
  (is (dl:pop-tail dlist) 4 "pop-tail is 4")
  (is (dl:to-list dlist) '(1 2 3) "list is '(1 2 3)")
  (is (dl:pop-tail dlist) 3 "pop-tail is 3")
  (is (dl:pop-tail dlist) 2 "pop-tail is 2")
  (is (dl:pop-tail dlist) 1 "pop-tail is 1")
  (is (dl:to-list dlist) nil "dlist is nil after last pop")
  (dl:push-tail dlist 1)
  (is (dl:pop-head dlist) 1 "pop-head is 1 after (push-tail dlist 1)")
  (is (dl:to-list dlist) nil "dlist is nil after last pop-head")
  (is (dl:len dlist) 0 "len dlist is 0 after last pop-head")
  (is (dl:pop-tail dlist) nil "pop-tail is nil when dlist is empty")
  (is (dl:pop-head dlist) nil "pop-head is nil when dlist is empty")
  (dl:push-tail dlist 1)
  (dl:push-tail dlist 2)
  (dl:push-tail dlist 3)
  (is (dl:peek-tail dlist) 3 "peek-tail now 3")
  (let ((even-value-node (dl:find-first-node dlist #'evenp)))
    (is (dl:value even-value-node) 2 "first even value is 2")
    (ok (dl:contains-node dlist even-value-node)
        "dlist contains given node")
    (dl:insert-before-node dlist even-value-node 1.5)
    (is (dl:to-list dlist) '(1 1.5 2 3)
        "insertion of 1.5 near midpoint succeeds")
    (dl:insert-after-node dlist even-value-node 2.5)
    (is (dl:to-list dlist) '(1 1.5 2 2.5 3))
    (is (dl:at dlist 1) 1.5 "second element is now 1.5")
    (dl:delete-node dlist even-value-node)
    (is (dl:to-list dlist) '(1 1.5 2.5 3)
        "list looks right after deleting the node with the even value")
    (ok (not (dl:delete-node dlist (make-instance 'dl:dlist-node
                                                         :prev nil
                                                         :next nil
                                                         :value 9)))
        "deleting a non-existing node from a dlist returns nil")))

(ok (not (let ((dlist (dl:from-list '(1 2 3))))
           (dl:delete-node-at dlist 5)))
    "delete-node-at returns nil when it sees an index that is out of range.")

(ok (loop with dlist = (make-instance 'dl:dlist)
          for a from 1 to 100 do (dl:push-tail dlist a)
          always (and (= (dl:len dlist) a)
                      (= (dl:peek-head dlist) 1)
                      (= (dl:peek-tail dlist) a)))
    "length of list adjusts correctly with push-tail")

(ok (loop with dlist = (make-instance 'dl:dlist)
          for a from 1 to 100 do (dl:push-head dlist a)
          always (and (= (dl:len dlist) a)
                      (= (dl:peek-head dlist) a)
                      (= (dl:peek-tail dlist) 1)))
    "length of list adjusts correctly with push-head")

(ok (let ((dlist (dl:from-list '(1 2 3))))
      (dl:delete-node-at dlist 1)
      (loop for node = (dl:head dlist) then (dl:next node)
            while node
            always (oddp (dl:value node))))
    "index-based node deletion works")

(ok (let* ((list (loop for a from 1 to 100 collect a))
           (dlist (dl:from-list list)))
      (loop for node = (dl:find-first-node dlist #'oddp)
            while node do (dl:delete-node dlist node))
      (loop for value in (dl:to-list dlist)
            always (evenp value)))
    "node-based node deletion works")

(let ((dlist (dl:from-list '(1 2 3))))
  (ok (and (null (dl:delete-node-at dlist -1))
           (null (dl:delete-node-at dlist 3)))
      "deleting nodes by index using out-of-bound index returns null")
  (is (dl:delete-node-at dlist 0) 1
      "deleting first node, with value 1, returns 1")
  (is (dl:delete-node-at dlist 0) 2
      "deleting first node, now with value 2, returns 2")
  (is (dl:delete-node-at dlist 0) 3
      "deleting first node, now with value 3, returns 3")
  (is (dl:delete-node-at dlist 0) nil
      "deleting first node in a list with no nodes returns nil")
  (dl:push-tail dlist 2)
  (dl:push-tail dlist 3)
  (dl:push-head dlist 1)
  (ok (loop for a from 1 to 3
            for node = (dl:head dlist) then (dl:next node)
            always (= (dl:value node) a))
      "adding values to a list that was previously emptied out works."))
(let* ((dlist (dl:from-list '(1 2 3)))
       (dlist-copy (dl:copy dlist)))
  (is (dl:len dlist) (dl:len dlist-copy)
      "copy produces dlist with same number of nodes as original")
  (is (dl:to-list dlist) (dl:to-list dlist-copy)
      "elements in copy are the same as elements in original")
  (dl:pop-head dlist)
  (isnt (dl:to-list dlist)
        (dl:to-list dlist-copy)
        "after popping original, elements not the same as elements in copy"))

(let* ((list '(3 2 4 1 5))
       (sorted-list (sort (copy-list list) #'<))
       (dlist (dl:from-list list))
       (sorted-dlist (dl:sorted dlist #'<)))
  (is sorted-list (dl:to-list sorted-dlist) "sorting works."))

(loop
  with list = '(0 1 2 3)
  with dlist = (dl:from-list list)
  for value in list
  for node = (dl:head dlist) then (dl:next node)
  for node-index = (dl:node-index node)
  do (is node-index value
         (if (< value 3)
             (format nil "node-index returns ~d for the ~:r node"
                     node-index (1+ value))
             (format nil "node-index returns ~d for the last node"
                     node-index))))

(finalize)
