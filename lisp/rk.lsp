1 группа. Выделить из смешанного структурированного списка числовые элементы, лежащие в заданном интервале. Из результата получить множество в виде одноуровн списка.
(defun between (a b lst) (mapcan
			#'(lambda (el) (if (numberp el) (and (> el a) (< el b) (cons el nil)) (if (listp el) (between a b el)))) lst))
(defun func (a b lst) (reduce #'(lambda (res el) (if (member el res) res (cons el res))) (between a b lst) :initial-value nil))
				            		            
(defun between_rec (a b lst) (cond ((null lst) nil)
		            ((and (numberp (car lst)) (> (car lst) a) (< (car lst) b)) (cons (car lst) (between_rec a b (cdr lst))))
		            ((listp (car lst)) (between_rec a b (car lst)))
		            (T(between_rec a b (cdr lst)))))
		            
		            
(defun func_rec_help (lst)(cond ((null lst) nil)
        		((member (car lst) (cdr lst)) (func_rec_help (cdr lst)))
        		(t (cons (car lst) (func_rec_help (cdr lst))))))
        		
(defun func_rec (a b lst) (func_rec_help(between_rec a b lst)))

3 группа. Дан одноуровневый смешанный список элементов, которые надо воспринимать как ключи, и структурированный смешанный список с информацией. Создать ассоциативную таблицу, проходя по верхнему уровню структурированного списка с информацией. Из полученной таблицы выделить таблицу с символьными ключами.

(defun conc (lst1 lst2) 
	(mapcar #'cons lst1 lst2))
	
(defun symb (lst) (mapcan #'(lambda (el) (if (symbolp (car el)) (cons el nil))) lst))

(defun func (lst1 lst2) (symb (conc lst1 lst2)))


(defun conc_rec (lst1 lst2) (if (and (not (null lst1)) (not (null lst2))) (cons (cons (car lst1) (if (listp (car lst2)) (cons (car lst2) nil) (car lst2))) (conc_rec (cdr lst1) (cdr lst2)))))
(defun symb_rec (lst) (cond ((null lst) nil)
		            ((symbolp (caar lst)) (cons (car lst) (symb_rec(cdr lst))))
		            (T(symb_rec (cdr lst)))))
		            
(defun func_rec (lst1 lst2) (symb_rec (conc_rec lst1 lst2)))
			
			
4 группа. Из многоуровневого списка получить одноуровневый и а начало добавить сумму числовых элементов (решение без добавления суммы)
(defun into_one (lst) (mapcan
			#'(lambda (el) (if (atom el) (cons el nil) (func el))) lst))
			
(defun into_one_rec (lst rst) (cond ((null lst) rst) 
				 ((atom lst) (cons lst rst))
				 ((into_one_rec (car lst) (into_one_rec (cdr lst) rst)))))
				 

2 группа. По-моему все отрицательные элементы смеш списка заменить на к, но не помню....
(defun minus_k (lst k) 
	(mapcar #'(lambda (x)
		(cond ((atom x) (cond ((numberp x) (- x k))(T x))) 
		(T(minus_k x k)))
	)lst))
	
(defun znak (lst)
	(mapcar #'(lambda (x)
			(cond 
			((atom x) (cond ((and (numberp x) (oddp x)) (- x))(t x))) 
			(T(znak x)))
)lst))


(defun check_el (lst)
	(if (> (find-if #'numberp lst) 10) T Nil))
	
(defun func (lst k) (znak (if (check_el lst) (minus_k lst k) lst)))

(defun minus_k_rec (lst k)
	(if (not(null lst)) 
		(cons 
			(if (atom (car lst)) 
				(if (numberp (car lst)) (- (car lst) k) (car lst)) 
				(minus_k_rec (car lst) k)
			) 
			(minus_k_rec (cdr lst) k)
		)
	)
)

(defun znak_rec (lst) 
	(if (not(null lst)) 
		(cons 
			(if (atom (car lst)) 
				(if (and (numberp (car lst)) (oddp (car lst))) (- (car lst)) (car lst)) 
				(znak_rec (car lst))
			) 
			(znak_rec (cdr lst))
		)
	)
)
(defun check_el_rec (lst) 
	(if (and (not(null lst)) (not(numberp (car lst)))) 
		(check_el (cdr lst)) 
		(if (and (not (null lst)) (>(car lst) 10)) T Nil)
	)
)
	
(defun func_rec (lst k) (znak_rec (if (check_el_rec lst) (minus_k_rec lst k) lst)))


5 группа. Есть два списка. Первый смешанный одноуровневый, второй состоит из смешанных одноуровневых. Нужно подсчитать сумму чисел тех подсписков второго списка, первые элементы которых входят в первый список
(defun one (lst_1 lst_2)
	(if (member (car lst_1) lst_2) T Nil))
(defun plus (lst) 
	(reduce #'+ (mapcar #'(lambda (x) (if (numberp x) x 0)) lst)))
(defun func (lst_1 lst_2) 
	(reduce #'+ (mapcar #'(lambda (x) (if (one x lst_1) (plus x) 0)) lst_2)))
	
	
(defun one_rec (lst_1 lst_2)
	(if (not (null lst_2)) (if (eql (car lst_1) (car lst_2)) T (one_rec lst_1 (cdr lst_2)))))
	

(defun plus_rec (lst)
	(if (null lst) 0 (+ (if (numberp (car lst)) (car lst) 0) (plus_rec (cdr lst)))))
	
(defun func_rec (lst1 lst2)
	(if (null lst2) 0  (+ (if (one_rec (car lst2) lst1) (plus_rec (car lst2)) 0) (func_rec lst1 (cdr lst2)))))
