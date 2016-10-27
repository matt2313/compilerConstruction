// Tests new and let statements used together

let int a := 10 in
new float b := 3.1415 in
int main()
{
    int ret := 0;
    
    let int c := 2 in
    let int d := 100 in
    ret := (a * b * c * d) as int;
    
    return ret; // 6283
}