float main()
{
    return (if(5 > 10)
    {
        return 200.;
    }
    else
    {
        return (if(20 / 5 == 4)
        {
            return (if(100 * 2 == 400)
            {
                return 400.;
            }
            else
            {
                return 3.14;
            });
        }
        else
        {
            return 100.;
        });
    });
}