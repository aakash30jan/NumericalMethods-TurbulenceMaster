 program simulator
        use point

        type(point_t) :: pt1, pt2
        real          :: offset

        pt2%x = 10
        pt2%y = 20
        pt2%z = 30
        offset = 100.0

        pt1 = pt2 + offset  ! step 3: using defined operator
        print *, pt1%x, pt1%y, pt1%z
end program simulator
