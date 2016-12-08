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
    
    return "When_you_divide_by_3_you_get_" ^ makePercent(divByThree(!x as float)) as string ^ "_of_the_total"; // "When you divide by 3 you get 33% of the total"
}