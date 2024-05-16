(in-package :dc-dlist)

(defclass dlist-node ()
  ((value :accessor value :initarg :value :initform nil)
   (prev :accessor prev :initarg :prev :initform nil)
   (next :accessor next :initarg :next :initform nil))
  (:documentation "Represents the node of a thread-safe, doubly-linked list."))


(defclass dlist ()
  ((head :accessor head :initform nil)
   (tail :accessor tail :initform nil)
   (len :accessor len :initform 0)
   (lock1 :accessor lock1 :initform nil)
   (lock2 :accessor lock2 :initform nil))
  (:documentation "Represents a thread-safe, doubly-linked list."))

(defmethod initialize-instance :after ((dlist dlist) &key)
  (setf (lock1 dlist) (make-mutex))
  (setf (lock2 dlist) (make-mutex)))

(defmethod push-head ((dlist dlist) (value t))
  "Push VALUE to the head of DLIST."
  (with-mutex ((lock1 dlist))
    (let ((old-head (head dlist))
          (new-head (make-instance 'dlist-node :value value)))
      (if old-head
          (progn
            (setf (head dlist) new-head
                  (next new-head) old-head
                  (prev old-head) new-head)
            (when (= (len dlist) 1)
              (setf (tail dlist) old-head))
            (incf (len dlist)))
          (setf (head dlist) new-head
                (tail dlist) new-head
                (len dlist) 1)))))

(defmethod push-tail ((dlist dlist) (value t))
  "Append VALUE to the tail of DLIST."
  (with-mutex ((lock1 dlist))
    (let ((old-tail (tail dlist))
          (new-tail (make-instance 'dlist-node :value value)))
      (if old-tail
          (progn
            (setf (next old-tail) new-tail
                  (tail dlist) new-tail
                  (prev new-tail) old-tail)
            (when (= (len dlist) 1)
              (setf (head dlist) old-tail))
            (incf (len dlist)))
          (setf (tail dlist) new-tail
                (head dlist) new-tail
                (len dlist) 1)))))

(defmethod pop-head ((dlist dlist))
  "Remove the first node of DLIST and return that node's value. If DLIST
has no nodes, this function returns NIL. If DLIST has a single node,
this function returns that node's value and makes DLIST empty. If
DLIST has more than one node, then this function removes the first node
and returns its value, making the second node of DLIST the head of
DLIST."
  (with-mutex ((lock1 dlist))
    (if (zerop (len dlist))
        nil
        (let ((value (value (head dlist))))
          (case (len dlist)
            (1 (setf (head dlist) nil
                     (tail dlist) nil
                     (len dlist) 0))
            (2 (setf (head dlist) (tail dlist)
                     (prev (head dlist)) nil
                     (len dlist) 1))
            (otherwise (setf (head dlist) (next (head dlist))
                             (prev (head dlist)) nil
                             (len dlist) (1- (len dlist)))))
          value))))

(defmethod pop-tail ((dlist dlist))
  "Remove the last node of DLIST and return that node's value. If DLIST
has no nodes, this function returns NIL. If DLIST has a single node,
this function returns that node's value and makes DLIST empty. If
DLIST has more than one node, then this function removes the last node
and returns its value, making the second node of DLIST the head of
DLIST."
  (with-mutex ((lock1 dlist))
    (if (zerop (len dlist))
        nil
        (let ((value (value (tail dlist))))
          (case (len dlist)
            (1 (setf (head dlist) nil
                     (tail dlist) nil
                     (len dlist) 0))
            (2 (setf (tail dlist) (head dlist)
                     (next (tail dlist)) nil
                     (len dlist) 1))
            (otherwise (setf (tail dlist) (prev (tail dlist)))
             (setf (next (tail dlist)) nil
                   (len dlist) (1- (len dlist)))))
          value))))

(defmethod peek-head ((dlist dlist))
  "Returns the value of of the first node in DLIST. If DLIST is empty,
this function returns NIL. This function does not change DLIST."
  (with-mutex ((lock1 dlist))
    (if (zerop (len dlist))
        nil
        (value (head dlist)))))

(defmethod peek-tail ((dlist dlist))
  "Returns the value of the last node in DLIST. If DLIST is empty,
this function returns NIL. This function does not change DLIST."
  (with-mutex ((lock1 dlist))
    (if (zerop (len dlist))
        nil
        (value (tail dlist)))))

(defmethod find-first-node ((dlist dlist) (comparison-function function))
  "Returns the first node in DLIST where calling COMPARISON-FUNCTION
with the value of the node returns T. If DLIST is empty or no node in
DLIST has a value that causes COMPARISON-FUNCTION to evaluate to T,
this function returns NIL."
  (with-mutex ((lock1 dlist))
    (loop for node = (head dlist) then (next node)
          while node
          when (funcall comparison-function (value node)) do (return node))))

(defmethod at ((dlist dlist) (index number))
  "Returns the value of the DLIST node at INDEX, where INDEX is
 zero-based. Calling this function with a value of 0 for INDEX is the
 same thing as calling the PEEK-HEAD function."
  (let ((node (node-at dlist index)))
    (when node (value node))))

(defmethod node-at ((dlist dlist) (index number))
  "Returns the node of DLIST at INDEX, where INDEX is zero-based. A
value of 0 for INDEX returns the first node in DLIST. if DLIST is
empty, this function returns NIL."
  (with-mutex ((lock1 dlist))
    (loop for node = (head dlist) then (next node)
          while node
          for i = 0 then (1+ i)
          when (= index i) do (return node))))

(defmethod node-index ((node dlist-node))
  (when node
    (loop for current = node then (prev current)
          for index = -1 then (1+ index)
          while current
          finally (return index))))

(defmethod insert-before-node ((dlist dlist) (existing-node dlist-node) (value t))
  "Creates a node with VALUE and inserts the new node into DLIST, at a
location immediately preceding EXISTING-NODE. If DLIST does not
contain EXISTING-NODE, no insertion occurs. This function returns the
new length of DLIST after the insertion."
  (with-mutex ((lock2 dlist))
    (when (contains-node dlist existing-node)
      (if (null (prev existing-node))
          (push-head dlist value)
          (with-mutex ((lock1 dlist))
            (let ((new-node (make-instance 'dlist-node
                                           :value value
                                           :prev (prev existing-node)
                                           :next existing-node)))
              (setf (next (prev new-node)) new-node
                    (prev (next new-node)) new-node
                    (len dlist) (1+ (len dlist)))))))))

(defmethod insert-after-node ((dlist dlist) (existing-node dlist-node) (value t))
  "Creates a node with VALUE and inserts it into DLIST, at a location
immediately following EXISTING-NODE. If DLIST does not contain
EXISTING-NODE, no insertion occurs. This function returns the new
length of DLIST after the insertion."
  (with-mutex ((lock2 dlist))
    (when (contains-node dlist existing-node)
      (if (null (next existing-node))
          (push-tail dlist value)
          (with-mutex ((lock1 dlist))
            (let ((new-node (make-instance 'dlist-node
                                           :value value
                                           :prev existing-node
                                           :next (next existing-node))))
              (setf (next (prev new-node)) new-node
                    (prev (next new-node)) new-node
                    (len dlist) (1+ (len dlist)))))))))

(defmethod delete-node ((dlist dlist) (node-to-delete dlist-node))
  "Deletes NODE-TO-DELETE from DLIST and returns the value of the
deleted node. If NODE-TO-DELETE does not exist in DLIST, no deletion
occurs."
  (with-mutex ((lock2 dlist))
    (when (contains-node dlist node-to-delete)
      (cond ((null (prev node-to-delete)) (pop-head dlist))
            ((null (next node-to-delete)) (pop-tail dlist))
            (t (with-mutex ((lock1 dlist))
                 (let ((value (value node-to-delete)))
                   (setf (next (prev node-to-delete)) (next node-to-delete)
                         (prev (next node-to-delete)) (prev node-to-delete)
                         (len dlist) (1- (len dlist)))
                   value)))))))

(defmethod delete-node-at ((dlist dlist) (index integer))
  "Deletes the DLIST node at INDEX and returns the value of the
deleted node. If INDEX is out of bounds for DLIST (INDEX < 0 or INDEX
>= (len DLIST)), then no deletion occurs and this function returns
NIL."
  (loop with deleted = nil and value = nil
        for i = 0 then (1+ i)
        for node = (head dlist) then (next node)
        while (and node (not deleted))
        when (= i index) do
          (setf value (value node))
          (delete-node dlist node)
          (setf deleted t)
        finally (return (when deleted value))))

(defun from-list (list)
  "Creates a DLIST doubly-linked list from LIST."
  (loop with dlist = (make-instance 'dlist)
        for element in list do (push-tail dlist element)
        finally (return dlist)))

(defmethod to-list ((dlist dlist))
  "Converts DLIST into a standard Lisp list."
  (with-mutex ((lock1 dlist))
    (loop for node = (head dlist) then (next node)
          while node
          collect (value node))))

(defmethod copy ((dlist dlist))
  "Creates a copy of DLIST. The nodes in the copy are new
objects. However, if the values in the original nodes are object
references, the copy will point to the same values."
  (with-mutex ((lock1 dlist))
    (loop with copy-of-dlist = (make-instance 'dlist)
          for node = (head dlist) then (next node)
          while node
          do (push-tail copy-of-dlist (value node))
          finally (return copy-of-dlist))))

(defmethod contains-node ((dlist dlist) (node dlist-node))
  "Returns a boolean value indicating if DLIST contains NODE."
  (loop for current-node = (head dlist) then (next current-node)
        while current-node thereis (equal current-node node)))

(defmethod sorted ((dlist dlist) (predicate function))
  "Returns a copy of DLIST that has its nodes sorted according to
the application of PREDICATE the the nodes' values. PREDICATE is
called with the values of the nodes in DLIST and works exactly like
the PREDICATE parameter of the standard SORT function in Lisp."
  (with-mutex ((lock2 dlist))
    (from-list (sort (to-list dlist) (lambda (a b) (funcall predicate a b))))))

(defmethod clear ((dlist dlist))
  "Makes DLIST empty, discarding any nodes it might contain."
  (with-mutex ((lock1 dlist))
    (setf (len dlist) 0
          (head dlist) nil
          (tail dlist) nil)))
