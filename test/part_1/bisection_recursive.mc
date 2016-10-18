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
float bisection_rec(float lowerBound, float upperBound, float threshold, int maxIterations)
{
    float midpoint := (lowerBound + upperBound) / 2.0;
    
    if(maxIterations == 0)
    {
        return midpoint_value;
    }
    
    float midpoint_value := f(midpoint);
    if(abs(midpoint_value) <= threshold)
    {
        return midpoint;
    }
    else
    {
        if(midpoint_value > 0)
        {
            return bisection_rec(lowerBound, midpoint, maxIterations - 1);
        }
        else
        {
            return bisection_rec(midpoint, upperBound, maxIterations - 1);
        }
    }
}

int main()
{
    print_float(bisection_rec(-5, -3, 0.0001, 25));
    return 0;
}