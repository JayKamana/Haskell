import Data.List
import Data.Function (on)

min_max :: [Int] -> (Int, Int)
min_max [] = (0,0)
min_max xs = ((minimum xs),(maximum xs))


totalgpa [] = 0.0
totalgpa (x:xs) = fs + totalgpa xs
 where
   fs = if (x=="A") then 4.0 else
    if (x=="A-") then 3.7 else
        if (x=="B+") then 3.3 else
            if (x=="B") then 3.0 else
                if (x=="B-") then 2.7 else
                    if (x=="C+") then 2.3 else
                        if (x=="C") then 2.0 else
                            if (x=="C-") then 1.7 else
                                if (x=="D+") then 1.3 else
                                    if (x=="D") then 1.0 else
                                        if (x=="D-") then 0.7 else 0.0

gpa xs = (totalgpa xs) / fromIntegral (length xs) :: Float

database =  [("ali", "cmpe100",85),("veli","cmpe200",98), ("sam", "cmpe111",86),("kevin","cmpe100",74),
             ("brianne","cmpe100",75),("mark","cmpe300",97),("cooper","cmpe400",88),("ben", "cmpe330",83),
             ("brianne","cmpe200",94), ("clark", "cmpe100",90),("sam", "cmpe300",82),("ali","cmpe111",100),
             ("kevin", "cmpe400",72),("smith","cmpe330",79),("cooper","cmpe100",81),("ali", "cmpe300",95),
             ("veli","cmpe300",99), ("ben", "cmpe400",73),("brian","cmpe111",87),("brianne","cmpe330",77)]

database1 =  [("ali", "cmpe200",85),("veli","cmpe200",97),("roxi", "cmpe200",20),("erfan", "cmpe200",36),("pedram","cmpe200",14),("roza", "cmpe200",57),("deli", "cmpe200",69),("hiba","cmpe200",60),("parisa", "cmpe200",33),("soha", "cmpe200",11),("veli","cmpe300",90),("roxi", "cmpe300",100),("ali", "cmpe300",22),("erfan","cmpe300",88),("pedram", "cmpe300",30),("soha", "cmpe300",75),("parisa","cmpe300",94),("roza", "cmpe300",10),("deli", "cmpe300",51),("hiba","cmpe300",43)]


snd2 :: (a,b,c) -> c
snd2 (a,b,c) = c

fst' :: (a, b, c) -> a
fst' (a,b,c) =  a
names [] = []
names (x:xs) = fst' x : names xs
totalStudents xs = nub (names xs)


secd :: (a,b,c) -> b
secd (a,b,c) = b
totalCourses [] = []
totalCourses (x:xs) = secd x : totalCourses xs
courses xs = nub (totalCourses xs)

lastTwo :: (a,b,c) -> (b,c)
lastTwo (a,b,c) = (b,c)
lastTwo' [] = []
lastTwo' (x:xs) = lastTwo x : lastTwo' xs
lastElems xs = lastTwo' xs

newList = sortBy (compare `on` fst) (lastElems database)
groupList = groupBy (\a b -> fst a == fst b) newList

checkEnrol [] = []
checkEnrol (x:xs) = (course,enrol) : checkEnrol xs
                   where
                        course = fst (head x)
                        enrol = length x  

enrolment xs = checkEnrol groupList
                     

totalGrade [] = 0
totalGrade (x:xs) = snd x + totalGrade xs

checkAverage [] = []
checkAverage (x:xs) = (course,average) : checkAverage xs
                       where 
                            course = fst (head x)
                            average = (totalGrade x) / fromIntegral (length x) :: Float

averageGrade xs = checkAverage groupList 

newList2 = sortBy (compare `on` secd) database
groupList2 = groupBy (\a b -> secd a == secd b) newList2

highestStudent xs = sortBy (flip compare `on` snd2) xs
                  
checkAchiever [] = []
checkAchiever (x:xs) = name : checkAchiever xs
                         where                          
                             name = fst' (head (highestStudent x))

highestAchiever xs = checkAchiever groupList2
