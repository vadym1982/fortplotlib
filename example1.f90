program example1
    use fortplotlib, only: fplot

    type(fplot) :: plt

    plt = fplot()
    call plt%plot([0d0, 1d0, 2d0, 3d0, 4d0], [0d0, 1d0, 4d0, 9d0, 16d0], ".-", "plot 1")
    call plt%plot([0d0, 1d0, 2d0, 3d0, 4d0], [0d0, -1d0, -2d0, -3d0, -4d0], ".-", "plot 2")
    call plt%title("Example")
    call plt%labels("X", "Y")
    call plt%show()
end program example1