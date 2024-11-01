module fortplotlib
    implicit none


    type fplot
        !--------------------------------------------------------------------------------------------------------------
        !! Class for points plot using GNUPlot
        !--------------------------------------------------------------------------------------------------------------
        integer, private                        :: series           !! Number of plot series
        integer, private                        :: max_count        !! Maximum number of series
        character(len=:), allocatable, private  :: plot_title       !! Chart title
        character(len=:), allocatable, private  :: x_lab            !! Label of horizontzl axis
        character(len=:), allocatable, private  :: y_lab            !! Label of vertical axis
        character(len=64), allocatable, private :: legends(:)       !! Legent items
        character(len=16), allocatable, private :: linestyles(:)    !! Linestyles for plot series
    contains
        procedure :: plot => fplot_plot       !! Add new series
        procedure :: show => fplot_show       !! Shot chart
        procedure :: title => fplot_title     !! Set chart title
        procedure :: labels => fplot_labels   !! Set axes labels
        procedure :: close => fplot_close     !! Remove all series and restore defaults
    end type fplot


    interface fplot
        procedure :: init
    end interface fplot

contains

    function init(max_count) result(self)
        !--------------------------------------------------------------------------------------------------------------
        !! `fplot` constructor
        !--------------------------------------------------------------------------------------------------------------
        integer, optional   :: max_count    !! Muximum number of series
        type (fplot)        :: self
        !--------------------------------------------------------------------------------------------------------------
        if (present(max_count)) then
            self%max_count = max_count
        else
            self%max_count = 16
        end if

        self%series = 0
        self%plot_title = ""
        self%x_lab = ""
        self%y_lab = ""
        allocate(self%legends(self%max_count), self%linestyles(self%max_count))
    end function init


    subroutine fplot_plot(self, x, y, linestyle, legend)
        !--------------------------------------------------------------------------------------------------------------
        !! Add new series with data points `x`, `y`
        !--------------------------------------------------------------------------------------------------------------
        class(fplot)                :: self
        real(kind=8)                :: x(:), y(:)   !! Data for points
        character(len=*), optional  :: linestyle    !! Style of line: ".", "-" or ".-"
        character(len=*), optional  :: legend       !! Legend for this series
        !--------------------------------------------------------------------------------------------------------------
        character(len=32) :: filename, tmp
        character(len=:), allocatable :: lg, ls
        integer :: n, i, k

        self%series = self%series + 1
        write(filename, "(I0,A)") self%series, ".txt"
        n = size(x, dim=1)
        if (size(y, dim=1) /= n) error stop "Arrays x and y must have the same size"

        if (present(legend)) then
            lg = legend
        else
            write(tmp, "(I0)") self%series
            lg = trim(tmp)
        end if

        k = min(len(lg), 64)
        self%legends(self%series) = lg

        if (present(linestyle)) then
            ls = linestyle
        else
            ls = "-"
        end if

        select case(linestyle)
            case (".")
                self%linestyles(self%series) = "points"
            case ("-")
                self%linestyles(self%series) = "lines"
            case (".-")
                self%linestyles(self%series) = "linespoints"
            case default
                self%linestyles(self%series) = "points"
        end select

        open (action='write', file=trim(filename), unit=1, status='replace')

        do i = 1, n
            write(1, *) x(i), y(i)
        end do

        close(1)
    end subroutine fplot_plot


    subroutine fplot_title(self, t)
        !--------------------------------------------------------------------------------------------------------------
        !! Set chart title as `t`
        !--------------------------------------------------------------------------------------------------------------
        class(fplot) :: self
        character(len=*) :: t
        !--------------------------------------------------------------------------------------------------------------
        self%plot_title = t
    end subroutine fplot_title


    subroutine fplot_labels(self, x_lab, y_lab)
        !--------------------------------------------------------------------------------------------------------------
        !! Set axes labels as `x_lab`, `y_lab`
        !--------------------------------------------------------------------------------------------------------------
        class(fplot)        :: self
        character(len=*)    :: x_lab, y_lab
        !--------------------------------------------------------------------------------------------------------------
        self%x_lab = x_lab
        self%y_lab = y_lab
    end subroutine fplot_labels


    subroutine fplot_show(self)
        !--------------------------------------------------------------------------------------------------------------
        !! Show chart with added series
        !--------------------------------------------------------------------------------------------------------------
        class(fplot) :: self
        character(len=32) :: filename
        integer :: i

        if (self%series < 1) error stop "Empty plot"
        open (action='write', file="plot.plt", unit=2, status='replace')

        write(2, '(A)') 'set title "' // self%plot_title // '"'
        write(2, '(A)') 'set grid'
        write(2, '(A)') 'set xlabel "' // self%x_lab // '"'
        write(2, '(A)') 'set ylabel "' // self%y_lab // '"'
        write(2, '(A)', advance='no') 'plot '

        do i = 1, self%series
            write(filename, "(I0,A)") i, ".txt"

            write(2, '(A,A,A,A)', advance='no') &
                    '"' // trim(filename) // '"', &
                    ' using 1:2 with ', &
                    trim(self%linestyles(i)), &
                    ' title "' // trim(self%legends(i)) // '"'

            if (i < self%series) write(2, '(A)', advance='no') ","
        end do

        close(2)
        call execute_command_line("gnuplot -p plot.plt")
    end subroutine fplot_show


    subroutine fplot_close(self)
        !--------------------------------------------------------------------------------------------------------------
        !! Remove all series and restore defaults
        !--------------------------------------------------------------------------------------------------------------
        class(fplot) :: self
        !--------------------------------------------------------------------------------------------------------------
        self%series = 0
    end subroutine fplot_close

end module fortplotlib
