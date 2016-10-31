// Tests nested functions
int main()
{
    float divByThree(float x)
    {
        return !x / 3.;
    }
    
    string makePercent(float x)
    {
        int makePercentValue(float x)
        {
            return (!x * 100.) as int;
        }
        
        return makePercentValue(!x) as string ^ "%";
    }
    
    int x := 1;
    
    return "When you divide by 3 you get " ^ makePercent(divByThree(!x as float)) as string ^ " of the total"; // "When you divide by 3 you get 33% of the total"
}