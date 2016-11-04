float two()
{
    return ((100 - 96) / 2) as float;
}

float pi()
{
    return 3.141592653;
}

let float final := two() * pi() in
float main()
{
    return final; // 6.283185306
}