module Lib
    (
    ) where

-- Câu 1
morseInCharacter :: Char -> [Char] -- Dịch 1 kí tự sang mã Morse
morseInCharacter str = if (str == 'A' || str == 'a') then ".- "
                        else if (str == 'B' || str == 'b') then "-... "
                        else if (str == 'C' || str == 'c') then "-.-. "
                        else if (str == 'D' || str == 'd') then "-.. "
                        else if (str == 'E' || str == 'e') then ". "
                        else if (str == 'F' || str == 'f') then "..-. "
                        else if (str == 'G' || str == 'g') then "--. "
                        else if (str == 'H' || str == 'h') then ".... "
                        else if (str == 'I' || str == 'i') then ".. "
                        else if (str == 'J' || str == 'j') then ".--- "
                        else if (str == 'K' || str == 'k') then "-.- "
                        else if (str == 'L' || str == 'l') then ".-.. "
                        else if (str == 'M' || str == 'm') then "-- "
                        else if (str == 'N' || str == 'n') then "-. "
                        else if (str == 'O' || str == 'o') then "--- "
                        else if (str == 'P' || str == 'p') then ".--. "
                        else if (str == 'Q' || str == 'q') then "--.- "
                        else if (str == 'R' || str == 'r') then ".-. "
                        else if (str == 'S' || str == 's') then "... "
                        else if (str == 'T' || str == 't') then "- "
                        else if (str == 'U' || str == 'u') then "..- "
                        else if (str == 'V' || str == 'v') then "...- "
                        else if (str == 'W' || str == 'w') then ".-- "
                        else if (str == 'X' || str == 'x') then "-..- "
                        else if (str == 'Y' || str == 'y') then "-.-- "
                        else if (str == 'Z' || str == 'z') then "--.. "
                        else if (str == '1') then ".---- "
                        else if (str == '2') then "..--- "
                        else if (str == '3') then "...-- "
                        else if (str == '4') then "....- "
                        else if (str == '5') then "..... "
                        else if (str == '6') then "-.... "
                        else if (str == '7') then "--... "
                        else if (str == '8') then "---.. "
                        else if (str == '9') then "----. "
                        else "-----"

morse :: [Char] -> [Char] -- Chạy kiểm tra từng kí tự xong gọi đệ quy
morse a = if (length a) > 0 then (morseInCharacter (a !! 0) ) ++ morse (drop 1 a)
            else ""  

--Câu 2:
catalan :: Integer -> Integer -- Hàm xuất ra số catalan thứ n
catalan n 
    | n <= 1 = 1
    | otherwise = sum [(catalan i) * (catalan $ n - i - 1)| i <- [0.. n - 1]]

check :: Integer -> Integer -> Bool --Hàm kiểm tra xem đây có phải là một số catalan hay không
check a b = if (catalan b) > a then False
            else if (catalan b) == a then True
            else check a (b + 1)

-- Chạy đệ quy từ đầu danh sách đến cuối danh sách kiểm tra 1 phần tử có là số catalan hay không
-- Nếu phải thì return 1 + countPerfect (drop 1 a) còn không thì countPerfect (drop 1 a) và điều kiện dừng là danh sách này rỗng
countPerfect :: [Integer] -> Integer 
countPerfect a = if (length a > 0 && (check (a !! 0) 0)) then 1 + countPerfect (drop 1 a)
                    else if (length a > 0 && (check (a !! 0) 0) == False) then countPerfect (drop 1 a)
                    else 0

                 
                        