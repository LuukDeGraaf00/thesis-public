// INTRO

float nearest(ray, triangles, left, right)
{
    difference = right - left;
    if (difference <= 64)
    {
        min4 = (1e30f, 1e30f, 1e30f, 1e30f);
        for (i = 0, i < difference / 4, i += 4)
        {
            min4 = min(min4, intersect(ray, triangles[left+i:left+i+3]));
        }
        min1 = min(min(min4.x, min4.y), min(min4.z, min4.w));
        for (i = 0, i < difference % 4, i++)
        {
            min1 = min(minimum, intersect(ray, triangles[((difference / 4) * 4) + i]));
        }
        return minimum;
    }
    else
    {
        mid = left + difference / 2;
        do in parallel
            l = nearest(ray, triangles, left, mid);
            r = nearest(ray, triangles, mid, right);
        return min(l, r);
    }
}

// MEMORY REPRESENTATION

struct PackedData       // 5 bytes
{
    char id;            // 1 byte
    int  data;          // 4 bytes
};

struct PaddedData       // 8 bytes
{
    char id;            // 1 byte
    char padding[3];    // 3 bytes
    int  data;          // 4 bytes
}; 







