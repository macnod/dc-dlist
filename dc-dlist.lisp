(in-package :dc-dlist)

(defclass dlist-node ()
  ((value :accessor value :initarg :value :initform nil)
   (prev :accessor prev :initarg :prev :initform nil)
   (next :accessor next :initarg :next :initform nil)))

(defclass dlist ()
  ((head :accessor head :initform nil)
   (tail :accessor tail :initform nil)
   (len :accessor len :initform 0)))

(defmethod push-head ((dlist dlist) (value t))
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
              (len dlist) 1))))

(defmethod push-tail ((dlist dlist) (value t))
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
              (len dlist) 1))))

(defmethod pop-head ((dlist dlist))
  (let ((value (value (head dlist))))
    (case (len dlist)
      (0 nil)
      (1 (setf (head dlist) nil
               (tail dlist) nil
               (len dlist) 0))
      (2 (setf (head dlist) (tail dlist)
               (prev (head dlist)) nil
               (len dlist) 1))
      (otherwise (setf (head dlist) (next (head dlist))
                       (prev (head dlist)) nil
                       (len dlist) (1- (len dlist)))))
    value))

(defmethod pop-tail ((dlist dlist))
  (let ((value (value (tail dlist))))
    (case (len dlist)
      (0 nil)
      (1 (setf (tail dlist) nil
               (len dlist) 0))
      (2 (setf (tail dlist) (head dlist)
               (next (tail dlist)) nil
               (len dlist) 1))
      (otherwise (setf (tail dlist) (prev (tail dlist)))
                 (setf (next (tail dlist)) nil
                       (len dlist) (1- (len dlist)))))
    value))

(defmethod peek-head ((dlist dlist))
  (if (zerop (len dlist))
      nil
      (value (head dlist))))

(defmethod peek-tail ((dlist dlist))
  (if (zerop (len dlist))
      nil
      (value (tail dlist))))

(defmethod find-first-node ((dlist dlist) (comparison-function function))
  (loop for node = (head dlist) then (next node)
     while node
     when (funcall comparison-function (value node)) do (return node)))

(defmethod at ((dlist dlist) (index number))
  (let ((node (node-at dlist index)))
    (when node (value node))))

(defmethod node-at ((dlist dlist) (index number))
  (loop for node = (head dlist) then (next node)
     while node
     for i = 0 then (1+ i)
     when (= index i) do (return node)))

(defmethod insert-before-node ((dlist dlist) (existing-node dlist-node) (value t))
  (when (contains-node dlist existing-node)
    (if (or (null existing-node) (null (prev existing-node)))
        (push-head dlist value)
        (let ((new-node (make-instance 'dlist-node 
                                       :value value
                                       :prev (prev existing-node)
                                       :next existing-node)))
          (setf (next (prev new-node)) new-node
                (prev (next new-node)) new-node
                (len dlist) (1+ (len dlist)))))))

(defmethod delete-node ((dlist dlist) (node-to-delete dlist-node))
  (when (contains-node dlist node-to-delete)
      (cond ((null (prev node-to-delete)) (pop-head dlist))
            ((null (next node-to-delete)) (pop-tail dlist))
            (t (let ((value (value node-to-delete)))
                 (setf (next (prev node-to-delete)) (next node-to-delete)
                       (prev (next node-to-delete)) (prev node-to-delete)
                       (len dlist) (1- (len dlist)))
                 value)))))

(defmethod delete-node-at ((dlist dlist) (index integer))
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
  (loop with dlist = (make-instance 'dlist)
     for element in list do (push-tail dlist element)
     finally (return dlist)))

(defmethod to-list ((dlist dlist))
  (loop for node = (head dlist) then (next node)
     while node
     collect (value node)))

(defmethod copy ((dlist dlist))
  (loop with copy-of-dlist = (make-instance 'dlist)
     for node = (head dlist) then (next node)
     while node 
     do (push-tail copy-of-dlist (value node))
     finally (return copy-of-dlist)))
       
(defmethod contains-node ((dlist dlist) (node dlist-node))
  (loop for current-node = (head dlist) then (next current-node)
     while current-node thereis (equal current-node node)))

