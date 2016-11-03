int main()
{
    string out := "";

    new int i := 0 in
    let int max := 10 in
    while((i := !i + 1) <= !max)
    {
        string s := "loop " ^ "number " ^ (!i as string) ^ " of " ^ (!max as string) ^ ". ";
    };
    
    return out;
}