package examples;

public class Mandelbrot
{
    public static float calcIteration(float xpx, float ypx, float max, float width, float height)
    {
        // Scale x & y to within (-2.5, -1) to (1, 1)
        float x0 = ((xpx / width) * (float)3.5) - (float)2.5;
        float y0 = ((ypx / height) * (float)2) - (float)1;

        float x = 0;
        float y = 0;

        float iteration = 0;
       
        while ((x * x) + (y * y) < (2 * 2) && iteration < max)
            {
                float xtemp = (x * x) - (y * y) + x0;
                y = (2 * x * y) + y0;
                x = xtemp;
                iteration += 1;
            }
        return iteration;
    }

    public static float[] calcMandelbrot(float[] arr, float width, float max)
    {
        float height = (float)arr.length / width;
        for (float y = 0; y < height; y ++) {
            for (float x = 0; x < width; x++) {
                int idx = (int)((y * width) + x);
                arr[idx] = calcIteration(x, y, max, width, height) / max;
            }
        }
        return arr;
    }

}
