type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let tt = Node(1,
	Node(2,
		Node(4,
			Empty,
			Empty
		),
		Empty
	),
	Node(3,
		Node(5,
			Empty,
			Node(6,
				Empty,
				Empty
			)
		),
		Empty
	)
);;

let  breadthBT tree = 
    let rec breadth_tail  = function 
    |[]->[]
    |Empty::tail -> breadth_tail tail 
 |Node(n, lSubtree, rSubtree) :: t -> n:: breadth_tail (t @( lSubtree:: rSubtree:: [])) in breadth_tail [tree];;

breadthBT tt;;
