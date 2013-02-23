package examples;

public class Vectors
{
    public static double square(double x)
    {
        return x * x;
    }

    public static double[] createBuffer(long size)
    {
        return new double[(int)size];
    }

    public static double length(double[] v, long size)
    {
        double sum = 0;
        for (long x = 0; x < size; x++)
            {
                sum += square(v[(int)x]);
            }
        return Math.sqrt(sum);
    }

    public static double[] normalize(double[] v, long size)
    {
        double len = length(v, size);
        for (long x = 0; x < size; x++)
            {
                v[(int)x] /= len; 
            }
        return v;
    }

}
