module arrayofpointers

    implicit none

    type, abstract :: shapes
        integer :: shape_type = 0
        contains
            procedure(getShapeType_interface), deferred :: getShapeType
    end type

    type container
        class(shapes), allocatable :: s
        double precision, dimension(:), allocatable :: array
        double precision, dimension(:), pointer :: ptr
    contains
        final :: destroy_container
    end type

    abstract interface
        integer function getShapeType_interface(this) result(shape_type)
            import :: shapes
            class(shapes), intent(in) :: this
            integer :: shapetype
        end function
    end interface

    interface shapes
        module procedure init
    end interface

    interface container
        module procedure init_container
    end interface

contains

    function init() result(this)
        class(shapes), pointer :: this
        this%shape_type = 0
    end function

    function init_container(n) result(this)
        type(container), target :: this
        integer, intent(in) :: n

        print '(A,I0,A)', achar(27)//'[32m'//"In constructor of container at ", loc(this), achar(27)//'[0m'
        if ( allocated(this%array) ) deallocate(this%array)
        allocate( this%array(n) )
        this%ptr => this%array(1:n/2)
    end function

    impure elemental subroutine destroy_container(this)
        type(container), intent(inout) :: this

        print '(A,I0,A)', achar(27)//'[31m'//"In destructor of container at ", loc(this), achar(27)//'[0m'
        if ( allocated(this%s    ) ) deallocate(this%s    )
        if ( allocated(this%array) ) deallocate(this%array)
        nullify( this%ptr )
    end subroutine

end module

module squaremod

    use arrayofpointers, only: shapes
    implicit none

    type, extends(shapes) :: square
        double precision :: side = 1.D0
        contains
            procedure :: getShapeType
            final     :: destroy
    end type

    interface square
        module procedure init
    end interface

contains

    function init(side) result(this)
        type(square) :: this
        double precision, intent(in) :: side
        this%shape_type = 2
        this%side = side
        print '(A,I0,A)', achar(27)//'[36m'//"In constructor of square at ", loc(this), achar(27)//'[0m'
    end function

    function getShapeType(this) result(shape_type)
        class(square), intent(in) :: this
        integer :: shape_type

        shape_type = this%shape_type
    end function

    impure elemental subroutine destroy(this)
        type(square), intent(inout) :: this
        print '(A,I0,A)', achar(27)//'[35m'//"In destructor of square at ", loc(this), achar(27)//'[0m'
    end subroutine

end module

module circlemod

    use arrayofpointers, only: shapes
    implicit none

    type, extends(shapes) :: circle
        double precision :: radius = 1.D0
        contains
            procedure :: getShapeType
            final     :: destroy
    end type

    interface circle
        module procedure init
    end interface

contains

    function init(radius) result(this)
        type(circle) :: this
        double precision, intent(in) :: radius
        this%shape_type = 1
        this%radius = radius
        print '(A,I0,A)', achar(27)//'[36m'//"In constructor of circle at ", loc(this), achar(27)//'[0m'
    end function

    function getShapeType(this) result(shape_type)
        class(circle), intent(in) :: this
        integer :: shape_type

        shape_type = this%shape_type
    end function

    impure elemental subroutine destroy(this)
        type(circle), intent(inout) :: this
        print '(A,I0,A)', achar(27)//'[35m'//"In destructor of circle at ", loc(this), achar(27)//'[0m'
    end subroutine

end module

module routines

    use arrayofpointers
    use circlemod
    use squaremod
    implicit none

contains

    subroutine print_shapes()
        type(container), dimension(:), allocatable, target :: myshapes
        type(container), allocatable :: dummy

        ! First allocate myshapes
        allocate(dummy, source=container(4))  ! Allocate a dummy to use as source
        allocate(myshapes(2), source=dummy)
        deallocate(dummy)                     ! Deallocate the dummy to avoid memory leaks

        myshapes(1)%ptr => myshapes(1)%array(1:3)

        print '(A,I0)', "size of myshapes(1)%array = ", size(myshapes(1)%array)
        print '(A,I0)', "size of myshapes(1)%ptr   = ", size(myshapes(1)%ptr  )
        print '(A,I0)', "size of myshapes(2)%array = ", size(myshapes(2)%array)
        print '(A,I0)', "size of myshapes(2)%ptr   = ", size(myshapes(2)%ptr  )

        print '(A,I0)', "Memory location of myshapes(1): ", loc(myshapes(1)%s)
        print '(A,I0)', "Memory location of myshapes(2): ", loc(myshapes(2)%s)


        ! Now allocate each member of myshapes as a different derived type
        allocate( myshapes(1)%s, source=circle(1.0D0) )
        allocate( myshapes(2)%s, source=square(2.0D0) )

        ! Check if members of myshapes were allocated correctly
        print '(A,I0)', "myshapes(1): shape_type = ", myshapes(1)%s%getShapeType()
        print '(A,I0)', "myshapes(2): shape_type = ", myshapes(2)%s%getShapeType()

        print '(A,I0)', "Memory location of myshapes(1): ", loc(myshapes(1)%s)
        print '(A,I0)', "Memory location of myshapes(2): ", loc(myshapes(2)%s)

        deallocate(myshapes)
    end subroutine

end module

program test_arrayofpointers

    use arrayofpointers
    use circlemod
    use squaremod
    use routines
    implicit none

    call print_shapes()

end program
