(in-package :dc-dlist)

(defclass dlist-node ()
  ((payload :accessor payload :initarg :payload :initform nil)
   (prev :accessor prev :initarg :prev :initform nil)
   (next :accessor next :initarg :next :initform nil)))

(defclass dlist ()
  ((head :accessor head :initform nil)
   (tail :accessor tail :initform nil)
   (len :accessor len :initform 0)
   (lock1 :accessor lock1 :initform nil)
   (lock2 :accessor lock2 :initform nil)))

(defmethod initialize-instance :after ((dlist dlist) &key)
  (setf (lock1 dlist) (make-mutex))
  (setf (lock2 dlist) (make-mutex)))

(defmethod push-head ((dlist dlist) (payload t))
  (with-mutex ((lock1 dlist))
    (let ((old-head (head dlist))
          (new-head (make-instance 'dlist-node :payload payload)))
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

(defmethod push-tail ((dlist dlist) (payload t))
  (with-mutex ((lock1 dlist))
    (let ((old-tail (tail dlist))
          (new-tail (make-instance 'dlist-node :payload payload)))
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
  (with-mutex ((lock1 dlist))
    (if (zerop (len dlist))
        nil
        (let ((payload (payload (head dlist))))
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
          payload))))

(defmethod pop-tail ((dlist dlist))
  (with-mutex ((lock1 dlist))
    (if (zerop (len dlist))
        nil
        (let ((payload (payload (tail dlist))))
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
          payload))))

(defmethod peek-head ((dlist dlist))
  (with-mutex ((lock1 dlist))
    (if (zerop (len dlist))
        nil
        (payload (head dlist)))))

(defmethod peek-tail ((dlist dlist))
  (with-mutex ((lock1 dlist))
    (if (zerop (len dlist))
        nil
        (payload (tail dlist)))))

(defmethod find-first-node ((dlist dlist) (comparison-function function))
  (with-mutex ((lock1 dlist))
    (loop for node = (head dlist) then (next node)
       while node
       when (funcall comparison-function (payload node)) do (return node))))

(defmethod at ((dlist dlist) (index number))
  (let ((node (node-at dlist index)))
    (when node (payload node))))

(defmethod node-at ((dlist dlist) (index number))
  (with-mutex ((lock1 dlist))
    (loop for node = (head dlist) then (next node)
       while node
       for i = 0 then (1+ i)
       when (= index i) do (return node))))

(defmethod insert-before-node ((dlist dlist) (existing-node dlist-node) (payload t))
  (with-mutex ((lock2 dlist))
    (when (contains-node dlist existing-node)
      (if (null (prev existing-node))
          (push-head dlist payload)
          (with-mutex ((lock1 dlist))
            (let ((new-node (make-instance 'dlist-node 
                                           :payload payload
                                           :prev (prev existing-node)
                                           :next existing-node)))
              (setf (next (prev new-node)) new-node
                    (prev (next new-node)) new-node
                    (len dlist) (1+ (len dlist)))))))))

(defmethod insert-after-node ((dlist dlist) (existing-node dlist-node) (payload t))
  (with-mutex ((lock2 dlist))
    (when (contains-node dlist existing-node)
      (if (null (next existing-node))
          (push-tail dlist payload)
          (with-mutex ((lock1 dlist))
            (let ((new-node (make-instance 'dlist-node
                                           :payload payload
                                           :prev existing-node
                                           :next (next existing-node))))
              (setf (next (prev new-node)) new-node
                    (prev (next new-node)) new-node
                    (len dlist) (1+ (len dlist)))))))))

(defmethod delete-node ((dlist dlist) (node-to-delete dlist-node))
  (with-mutex ((lock2 dlist))
    (when (contains-node dlist node-to-delete)
      (cond ((null (prev node-to-delete)) (pop-head dlist))
            ((null (next node-to-delete)) (pop-tail dlist))
            (t (with-mutex ((lock1 dlist))
                 (let ((payload (payload node-to-delete)))
                   (setf (next (prev node-to-delete)) (next node-to-delete)
                         (prev (next node-to-delete)) (prev node-to-delete)
                         (len dlist) (1- (len dlist)))
                   payload)))))))

(defmethod delete-node-at ((dlist dlist) (index integer))
  (loop with deleted = nil and payload = nil
     for i = 0 then (1+ i)
     for node = (head dlist) then (next node)
     while (and node (not deleted))
     when (= i index) do 
       (setf payload (payload node))
       (delete-node dlist node) 
       (setf deleted t)
     finally (return (when deleted payload))))

(defun from-list (list)
  (loop with dlist = (make-instance 'dlist)
     for element in list do (push-tail dlist element)
     finally (return dlist)))

(defmethod to-list ((dlist dlist))
  (with-mutex ((lock1 dlist))
    (loop for node = (head dlist) then (next node)
       while node
       collect (payload node))))

(defmethod copy ((dlist dlist))
  (with-mutex ((lock1 dlist))
    (loop with copy-of-dlist = (make-instance 'dlist)
       for node = (head dlist) then (next node)
       while node 
       do (push-tail copy-of-dlist (payload node))
       finally (return copy-of-dlist))))

(defmethod contains-node ((dlist dlist) (node dlist-node))
  (loop for current-node = (head dlist) then (next current-node)
     while current-node thereis (equal current-node node)))

(defmethod sorted ((dlist dlist) (predicate function))
  (with-mutex ((lock2 dlist))
    (from-list (sort (to-list dlist) (lambda (a b) (funcall predicate a b))))))

(defmethod clear ((dlist dlist))
  (with-mutex ((lock1 dlist))
    (setf (len dlist) 0
          (head dlist) nil
          (tail dlist) nil)))
