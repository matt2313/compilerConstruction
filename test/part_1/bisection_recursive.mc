/* Program that finds an approximation of the root of an equasion */

float abs(float x)
{
    float returnVal := 0.;
    
    if(x >= 0.)
    {
        returnVal := !x;
    }
    else
    {
        returnVal := -!x;
    };
    
    return returnVal;
}

float f(float x)
{
    return !x * !x * !x + 3. * !x * !x - 2. * !x + 5.;
}

// f(lowerBound) must be negative, f(upperBound) must be positive
float bisection_rec(float lowerBound, float upperBound, float threshold, int maxIterations)
{
    float returnVal := -1.;
    float midpoint := (!lowerBound + !upperBound) / 2.0;
    
    if(maxIterations == 0)
    {
        returnVal := !midpoint_value;
    };
    
    float midpoint_value := f(!midpoint);
    if(abs(!midpoint_value) <= !threshold)
    {
        returnVal := !midpoint;
    }
    else
    {
        if(midpoint_value > 0.)
        {
            returnVal := bisection_rec(!lowerBound, !midpoint, !threshold, !maxIterations - 1);
        }
        else
        {
            returnVal := bisection_rec(!midpoint, !upperBound, !threshold, !maxIterations - 1);
        };
    };
    
    return !returnVal;
}

float main()
{
    return bisection_rec(-5., -3., 0.0001, 25);
}