int main()
{
    1 as float; // 1.0
    10 as string; // "10"
    0 as bool; // false
    
    1.0 as int; // 1
    2.9 as int; // 2
    3.14 as string; // "3.14"
    1.0 as bool; // true
    
    "1" as int; // 1
    "10.5" as float; //10.5
    "true" as bool; //true
    
    false as int; // 0
    true as float; // 1.0
    false as string; // "false"
    
    (10 + 10) as string; // 20
    ((10.0 * 10.0) as int + 5) as string; // "105"
    3.14 as string ^ " " ^ 10 as string ^ " " ^ false as string; // "3.14 10 false"

    3 == 3.0; // true
    1 != 1f; // false
    2 > 1.5; // true
    3.14 < 1; // false
    12.0 <= 12; // true
    100f >= 200; // false
    
    return false as int;
}