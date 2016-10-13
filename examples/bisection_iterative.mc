/* Program that finds an approximation of the root of an equasion */

float abs(float x)
{
    if(x >= 0)
    {
        return x;
    }
    else
    {
        return -x;
    }
}

float f(float x)
{
    return x * x * x + 3 * x * x - 2 * x + 5;
}

// f(lowerBound) must be negative, f(upperBound) must be positive
float bisection_it(float a_lowerBound, float a_upperBound, float threshold, int a_maxIterations)
{
    float lowerBound := a_lowerBound;
    float upperBound := a_upperBound;
    int maxIterations := a_maxIterations;
    
    while(maxIterations > 0)
    {
        float midpoint := (lowerBound + upperBound) / 2.0;        
        float midpoint_value := f(midpoint);
    
        if(abs(midpoint_value) <= threshold)
        {
            return midpoint;
        }
        else
        {
            if(midpoint_value > 0)
            {
                upperBound := midpoint;
            }
            else
            {
                lowerBound := midpoint;
            }
        }
        
        maxIterations := maxIterations - 1;
    }
}

int main()
{
    print_float(bisection_it(-5, -3, 0.0001, 25));
    return 0;
}