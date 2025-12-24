;; Tests for dc-dlist

(require :fiveam)

(push (uiop:getcwd) asdf:*central-registry*)
(ql:register-local-projects)
(asdf:load-system :dc-dlist)

(defpackage :dc-dlist-tests
  (:use :cl :fiveam)
  (:local-nicknames (:dl :dc-dlist)))

(in-package :dc-dlist-tests)

(def-suite dc-dlist-suite :description "Tests for dc-dlist")

(in-suite dc-dlist-suite)

(test group-1
  (let ((dlist (dl:from-list '(1 2 3))))
    (is (equal '(1 2 3) (dl:to-list dlist)) "expected ~a; got ~a")
    (is (equal 2 (dl:at dlist 1)) "at 1 of '(1 2 3)) is 2")
    (is (equal 1 (dl:at dlist 0)) "at 0 of '(1 2 3)) is 1")
    (is (equal 3 (dl:at dlist 2)) "at 2 of '(1 2 3)) is 3")
    (is-false (dl:at dlist -1) "at -1 of '(1 2 3)) is nil")
    (is-false (dl:at dlist 3) "at 3 of '(1 2 3)) is nil")
    (dl:push-head dlist 0)
    (is (equal '(0 1 2 3) (dl:to-list dlist))
      "push-head '(1 2 3) 0 -> '(0 1 2 3)")
    (dl:push-tail dlist 4)
    (is (equal '(0 1 2 3 4) (dl:to-list dlist))
      "push-tail '(0 1 2 3) 4 -> '(0 1 2 3 4)")
    (is-true (zerop (dl:peek-head dlist))
      "peek-head '(0 1 2 3 4) -> 0)")
    (is-true (zerop (dl:pop-head dlist))
      "pop-head '(0 1 2 3 4) -> 0, and list diminished")
    (is (equal 1 (dl:peek-head dlist)) "peek-head '(1 2 3 4) -> 1")
    (is (equal '(1 2 3 4) (dl:to-list dlist)) "list is '(1 2 3 4)")
    (is (equal 4 (dl:peek-tail dlist)) "peek-tail is 4")
    (is (equal 4 (dl:pop-tail dlist)) "pop-tail is 4")
    (is (equal '(1 2 3) (dl:to-list dlist)) "list is '(1 2 3)")
    (is (equal 3 (dl:pop-tail dlist)) "pop-tail is 3")
    (is (equal 2 (dl:pop-tail dlist)) "pop-tail is 2")
    (is (equal 1 (dl:pop-tail dlist)) "pop-tail is 1")
    (is-false (dl:to-list dlist) "dlist is nil after last pop")
    (dl:push-tail dlist 1)
    (is (equal 1 (dl:pop-head dlist)) "pop-head is 1 after (push-tail dlist 1)")
    (is-false (dl:to-list dlist) "dlist is nil after last pop-head")
    (is-true (zerop (dl:len dlist)) "len dlist is 0 after last pop-head")
    (is-false (dl:pop-tail dlist) "pop-tail is nil when dlist is empty")
    (is-false (dl:pop-head dlist) "pop-head is nil when dlist is empty")
    (dl:push-tail dlist 1)
    (dl:push-tail dlist 2)
    (dl:push-tail dlist 3)
    (is (equal 3 (dl:peek-tail dlist)) "peek-tail now 3")
    (let ((even-value-node (dl:find-first-node dlist #'evenp)))
      (is (equal 2 (dl:value even-value-node)) "first even value is 2")
      (is-true (dl:contains-node dlist even-value-node)
        "dlist contains given node")
      (dl:insert-before-node dlist even-value-node 1.5)
      (is (equal '(1 1.5 2 3) (dl:to-list dlist))
        "insertion of 1.5 near midpoint succeeds")
      (dl:insert-after-node dlist even-value-node 2.5)
      (is (equal '(1 1.5 2 2.5 3) (dl:to-list dlist))
        "expected ~a; got ~a" '(1 1.5 2 2.5 3) (dl:to-list dlist))
      (is (equal 1.5 (dl:at dlist 1)) "second element is now 1.5")
      (dl:delete-node dlist even-value-node)
      (is (equal '(1 1.5 2.5 3) (dl:to-list dlist))
        "list looks right after deleting the node with the even value")
      (is-false (dl:delete-node
                  dlist
                  (make-instance 'dl:dlist-node :prev nil :next nil :value 9))
        "deleting a non-existing node from a dlist returns nil"))))

(test delete-out-of-range
  (is-false (let ((dlist (dl:from-list '(1 2 3))))
              (dl:delete-node-at dlist 5))
    "delete-node-at returns nil when it sees an index that is out of range."))

(test length-after-push
  (is-true (loop with dlist = (make-instance 'dl:dlist)
             for a from 1 to 100 do (dl:push-tail dlist a)
             always (and (= (dl:len dlist) a)
                      (= (dl:peek-head dlist) 1)
                      (= (dl:peek-tail dlist) a)))
    "length of list adjusts correctly with push-tail")

  (is-true (loop with dlist = (make-instance 'dl:dlist)
             for a from 1 to 100 do (dl:push-head dlist a)
             always (and (= (dl:len dlist) a)
                      (= (dl:peek-head dlist) a)
                      (= (dl:peek-tail dlist) 1)))
    "length of list adjusts correctly with push-head"))

(test deleteion
  (is-true (let ((dlist (dl:from-list '(1 2 3))))
             (dl:delete-node-at dlist 1)
             (loop for node = (dl:head dlist) then (dl:next node)
               while node
               always (oddp (dl:value node))))
    "index-based node deletion works")

  (is-true (let* ((list (loop for a from 1 to 100 collect a))
                   (dlist (dl:from-list list)))
             (loop for node = (dl:find-first-node dlist #'oddp)
               while node do (dl:delete-node dlist node))
             (loop for value in (dl:to-list dlist)
               always (evenp value)))
    "node-based node deletion works")

  (let ((dlist (dl:from-list '(1 2 3))))
    (is-true (and (null (dl:delete-node-at dlist -1))
               (null (dl:delete-node-at dlist 3)))
      "deleting nodes by index using out-of-bound index returns null")
    (is (equal 1 (dl:delete-node-at dlist 0))
      "deleting first node, with value 1, returns 1")
    (is (equal 2 (dl:delete-node-at dlist 0))
      "deleting first node, now with value 2, returns 2")
    (is (equal 3 (dl:delete-node-at dlist 0))
      "deleting first node, now with value 3, returns 3")
    (is-false (dl:delete-node-at dlist 0)
      "deleting first node in a list with no nodes returns nil")
    (dl:push-tail dlist 2)
    (dl:push-tail dlist 3)
    (dl:push-head dlist 1)
    (is-true (loop for a from 1 to 3
               for node = (dl:head dlist) then (dl:next node)
               always (= (dl:value node) a))
      "adding values to a list that was previously emptied out works.")))

(test copy
  (let* ((dlist (dl:from-list '(1 2 3)))
          (dlist-copy (dl:copy dlist)))
    (is (equal (dl:len dlist-copy) (dl:len dlist))
      "copy produces dlist with same number of nodes as original")
    (is (equal (dl:to-list dlist-copy) (dl:to-list dlist))
      "elements in copy are the same as elements in original")
    (dl:pop-head dlist)
    (is (not (equal (dl:to-list dlist)
               (dl:to-list dlist-copy)))
      "after popping original, elements not the same as elements in copy")))

(test sorting
  (let* ((list '(3 2 4 1 5))
          (sorted-list (sort (copy-list list) #'<))
          (dlist (dl:from-list list))
          (sorted-dlist (dl:sorted dlist #'<)))
    (is (equal (dl:to-list sorted-dlist) sorted-list) "sorting works.")))

(test node-index
  (loop
    with list = '(0 1 2 3)
    with dlist = (dl:from-list list)
    for value in list
    for node = (dl:head dlist) then (dl:next node)
    for node-index = (dl:node-index node)
    do (is (equal node-index value)
         (if (< value 3)
           (format nil "node-index returns ~d for the ~:r node"
             node-index (1+ value))
           (format nil "node-index returns ~d for the last node"
             node-index)))))

;;; Run tests
(unless (run-all-tests)
  (sb-ext:quit :unix-status 1))
