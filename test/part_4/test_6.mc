let int a := 1 in
let int b := (!a + 1) * 2 in
let int c := !b * 15 in
int main()
{
    return !a + !b + !c; // 321
}