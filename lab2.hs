min_max list1 = (minimum list1, maximum list1)

findltgrd [] = []
findltgrd xs = [y|(x,y)<- zip ["A","A-","B+","B","B-","C+","C","C-","D+","D","D-","F"] [4,3.7,3.3,3,2.7,2.3,2,1.7,1.3,1,0.7,0], (head xs)==x] ++ findltgrd (tail xs)
gpa list1 = realToFrac (sum (findltgrd list1)) / realToFrac (length list1)

database1 =  [("ali", "cmpe200",85),("veli","cmpe200",97),("roxi", "cmpe200",20),("erfan", "cmpe200",36),("pedram","cmpe200",14),("roza", "cmpe200",57),("deli", "cmpe200",69),("hiba","cmpe200",60),("parisa", "cmpe200",33),("soha", "cmpe200",11),("veli","cmpe300",90),("roxi", "cmpe300",100),("ali", "cmpe300",22),("erfan","cmpe300",88),("pedram", "cmpe300",30),("soha", "cmpe300",75),("parisa","cmpe300",94),("roza", "cmpe300",10),("deli", "cmpe300",51),("hiba","cmpe300",43)]

database2 =  [("ali", "cmpe200",85),("veli","cmpe200",97),("roxi", "cmpe200",20),("erfan", "cmpe200",36),("pedram","cmpe200",14),("roza", "cmpe200",57),("deli", "cmpe200",69),("hiba","cmpe200",60),("parisa", "cmpe200",33),("soha", "cmpe200",11),("sohyla","cmpe200",83),("abbas", "cmpe200",100),("ghazal", "cmpe200",22),("erfan","cmpe300",88),("pedram", "cmpe300",30),("roxi", "cmpe300",75),("parisa","cmpe300",94),("roza", "cmpe300",10),("deli", "cmpe300",51),("hiba","cmpe300",43)]
get1st (a,_,_) = a
get2nd (_,a,_) = a
get3rd (_,_,a) = a

revertup [] = []
revertup ((x,y,z):xs) = (z,y,x) : revertup xs
highestAchiever list1 = get3rd(maximum (revertup list1))

removeItem _ []                 = []
removeItem x (y:ys) | get1st x == get1st y    = removeItem x ys
                    | otherwise = y : removeItem x ys


newlist1 [] = []
newlist1 (x:xs) = x : newlist1 (removeItem x xs)

totalStudents list1 = length (newlist1 list1)

cmpe200enrolment [] = 0
cmpe200enrolment list1
	| get2nd(head list1) == "cmpe200"  = 1 + cmpe200enrolment (tail list1)
	| otherwise                        = cmpe200enrolment (tail list1)

cmpe300enrolment [] = 0
cmpe300enrolment list1
	| get2nd(head list1) == "cmpe300"  = 1 + cmpe300enrolment (tail list1)
	| otherwise                        = cmpe300enrolment (tail list1)

enrolment list1 = [("cmpe200",cmpe200enrolment list1),("cmpe300",cmpe300enrolment list1)]

cmpe200Grade [] = 0
cmpe200Grade list1
	| get2nd(head list1) == "cmpe200"  =  get3rd(head list1) + cmpe200Grade (tail list1)
	| otherwise                        = cmpe200Grade (tail list1) 

cmpe300Grade [] = 0
cmpe300Grade list1
	| get2nd(head list1) == "cmpe300"  =  get3rd(head list1) + cmpe300Grade (tail list1)
	| otherwise                        = cmpe300Grade (tail list1)

averageGrade list1 = [("cmpe200",realToFrac(cmpe200Grade list1)/realToFrac(cmpe200enrolment list1)),("cmpe300",realToFrac(cmpe300Grade list1)/realToFrac(cmpe300enrolment list1))]


