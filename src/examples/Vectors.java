package examples;

public static class Vectors
{
    public static double square(double x)
    {
        return x * x;
    }

    public static double[] createBuffer(long size)
    {
        return new double[size];
    }

    public static double length(double[] v, long size)
    {
        double sum = 0;
        for (long x = 0; x < size; x++)
            {
                sum += square(v[x]);
            }
        return Math.sqrt(sum);
    }

    public static double[] normalize(double[] v, long size)
    {
        double len = length(v, size);
        for (long x = 0; x < size; x++)
            {
                v[x] /= len; 
            }
        return v;
    }

}
