program example2
    use fortplotlib, only: fplot

    type(fplot) :: plt
    real(kind=8), allocatable :: x(:), y(:), r(:)
    real(kind=8) :: at = 0.5d0, bt = 1.0d0, a, b, x_mean, y_mean, denom
    integer :: n = 50

    x = [(dble(i) / n, i = 1, n)]
    print *, x
    y = at * x + bt
    allocate(r(n))
    call random_number(r)
    r = (r - 0.5d0) * 0.15
    y = y + r

    x_mean = sum(x) / n
    y_mean = sum(y) / n
    denom = sum((x - x_mean) ** 2)

    a = sum((x - x_mean) * (y - y_mean)) / sum((x - x_mean) ** 2)
    b = y_mean - a * x_mean

    plt = fplot()
    call plt%plot(x, y, ".", "Data")
    call plt%plot(x, a * x + b, "-", "Fit result")
    call plt%title("Linear regression")
    call plt%labels("x", "y")
    call plt%show()
end program example2