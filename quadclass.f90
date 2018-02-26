module quad

    use Image, only : RGB

    implicit none

    type :: quadtree
        integer :: max_level = 1, level
        integer :: width, height
        type(RGB) :: data
        real :: errorval
        ! integer :: xlbound, xhbound, ylbound, yhbound 
        character(len=:), allocatable :: code
        type(quadtree), pointer :: children(:)
    contains
        procedure :: create  => create_sub
    end type quadtree

    public
    private :: create_sub

    contains
    
        subroutine create_sub(this, array)
        !   create quadtree

            use utils, only : str
            use Image, only : RGBimage

            implicit none

            class(quadtree), intent(INOUT) :: this
            type(RGBimage),  intent(IN)    :: array

            logical :: divide
            integer :: section

            this%width  = array%width
            this%height = array%height

            if(this%width /= this%height)error stop 'height /= width'
            if(mod(this%width, 2) /= 0)error stop 'array not div by 2'

            section = this%width/2
            this%level = int(log(real(this%width)) / log(2.))

            if(this%level < this%max_level)then
                this%children => null()
                this%data = AverageImageColor(array)
                return
            end if

            divide = subdivide(array, this%errorval)
            this%code = '0'

            if(divide)then
                !create children 

                ! do i = 0,3
                !     this%children(i+1)%xlbound = mod(i,2)*section+1
                !     this%children(i+1)%xhbound = section+section*mod(i,2)
                !     if(i < 2)then
                !         this%children(i+1)%ylbound = 1
                !         this%children(i+1)%yhbound = section     
                !     else
                !         this%children(i+1)%ylbound = section+1
                !         this%children(i+1)%yhbound = section*2                                   
                !     end if
                ! end do

                call addNode(this%children(1), array%colour(1:section, 1:section), this%code//str(0), this%errorval)
                call addNode(this%children(2), array%colour(1:section, section+1:section*2), this%code//str(1), this%errorval)
                call addNode(this%children(3), array%colour(section+1:section*2, 1:section), this%code//str(2), this%errorval)
                call addNode(this%children(4), array%colour(section+1:section*2, section+1:section*2), this%code//str(3), &
                             this%errorval)
            else
                !create leaf
                this%children => null()
                this%data = AverageImageColor(array)
            end if

        end subroutine create_sub


        recursive subroutine getNode(node, x, y)

            implicit none
            
            type(quadtree), intent(IN) :: node
            integer,        intent(IN) :: x, y

            if(associated(node%children))then
                print*,node%children(1)%width,node%children(1)%level
                call getNode(node%children(3), x, y)
            end if
        end subroutine getNode


        recursive subroutine addNode(node, carray, string, errorval)
        ! recursivly add a node

            use utils, only : str
            use Image, only : RGBimage, RGB, alloc_image

            implicit none

            type(quadtree), intent(INOUT) :: node
            type(RGB),      intent(IN)    :: carray(:, :)
            character(*),   intent(IN)    :: string
            real,           intent(IN)    :: errorval

            logical        :: divide
            integer        :: section
            type(RGBimage) :: array


            call alloc_image(array, size(carray, 1), size(carray, 2))

            array%colour = carray

            node%width  = array%width
            node%height = array%height
            node%errorval = errorval

            if(node%width /= node%height)error stop 'height /= width'
            if(mod(node%width, 2) /= 0)then
                if(node%width /= 1)error stop 'array not div by 2'
            end if

            section = node%width/2

            !get node level via logarithms. i.e. conversion of into log_2
            node%level = int(log(real(node%width)) / log(2.))
            node%code = string

            if(node%level < node%max_level)then
                node%children => null()
                node%data = AverageImageColor(array)
                return
            end if

            divide = subdivide(array, node%errorval)

            if(divide)then
                !create children
                allocate(node%children(4))
                call addNode(node%children(1), array%colour(1:section, 1:section), node%code//str(0), node%errorval)
                call addNode(node%children(2), array%colour(1:section, section+1:section*2), node%code//str(1), node%errorval)
                call addNode(node%children(3), array%colour(section+1:section*2, 1:section), node%code//str(2), node%errorval)
                call addNode(node%children(4), array%colour(section+1:section*2, section+1:section*2), node%code//str(3), &
                             node%errorval)
            else
                !create leaf
                node%children => null()
                node%data =  AverageImageColor(array)
            end if
        end subroutine addNode


        recursive subroutine printTree(node, indent)
        !   recursive routine to print tree

            implicit none

            type(quadtree), intent(INOUT) :: node
            integer,        intent(IN)    :: indent

            integer :: i

            ! print indent and the code for the node
            write(*,'(A,1X,a)',advance='no')repeat(' ', indent),node%code

            !recurse down tree if there are children
            if(associated(node%children))then
                write(*,*)
                do i = 1, 4
                    call printTree(node%children(i), indent + 1)
                end do
            else
                !else write node data on same line
                write(*,'(3(1X,I3.1))')node%data
            end if
        end subroutine printTree


        recursive subroutine display(node, carray, flag)
        !   show nodes as a picture

            use Image, only : RGBimage, RGB, alloc_image

            implicit none

            logical,        intent(IN),   optional :: flag
            type(quadtree), intent(INOUT)          :: node
            type(RGB),      intent(OUT)            :: carray(:, :)

            integer :: section


            if(associated(node%children))then
                section = node%width/2
                call display(node%children(1), carray(1:section, 1:section), flag)
                call display(node%children(2), carray(1:section, section+1:section*2), flag)
                call display(node%children(3), carray(section+1:section*2, 1:section), flag)
                call display(node%children(4), carray(section+1:section*2, section+1:section*2), flag)
            else
                carray = node%data

                if(present(flag) .and. flag )then!.and. node%level /= node%max_level
                    carray(:, size(carray, 2)) = RGB(0, 0, 0)
                    carray(:, 1) = RGB(0, 0, 0)
                    carray(size(carray, 1) , :) = RGB(0, 0, 0)
                    carray(1, :) = RGB(0, 0, 0)
                end if
            end if

        end subroutine display


        recursive subroutine delete_tree(node)

            implicit none

            type(quadtree), intent(INOUT) :: node

            integer :: i

            if(associated(node%children))then
                do i = 1, 4
                    call delete_tree(node%children(i))
                    deallocate(node%children(i)%code)
                end do
                deallocate(node%children)
            end if
        end subroutine delete_tree


        logical function subdivide(area, errorval)
        !   subdivide node based upon a metric

            use Image, only : RGBimage, RGB

            implicit none

            type(RGBimage), intent(IN) :: area
            real,           intent(IN) :: errorval

            type(RGB) :: avgcolour
            integer   :: i, j
            real      :: error, dg, dr, db, r, term

            error = 0.
            subdivide = .false.
            avgcolour = AverageImageColor(area)

            do i = 1, area%height
                do j = 1, area%width
                    dr =  area%colour(j, i)%red - avgcolour%red
                    dg = area%colour(j, i)%green - avgcolour%green
                    db = area%colour(j, i)%blue - avgcolour%blue
                    r = area%colour(j, i)%red - avgcolour%red / 2.
                    term = sqrt(2.*dr**2 + 4.*dg**2 + (2. + ((255. - r)/256.)*db**2 ))
                    error = error + term
                end do
            end do

            error = sqrt(error / (area%width*area%height*3))
            ! print*,error
            if(error > errorval)then
                subdivide = .true.
            end if

        end function subdivide


        type(RGB) function AverageImageColor(img)

            use Image, only : RGB, RGBimage

            implicit none

            type(RGBimage), intent(IN) :: img
            
            integer :: x, y
            real    :: r, g, b

            r = 0
            g = 0
            b = 0

            do x = 1, img%width
                do y = 1, img%height
                    r = r + img%colour(x, y)%red
                    g = g + img%colour(x, y)%green
                    b = b + img%colour(x, y)%blue
                end do
            end do

            r = r / (img%width*img%height)
            g = g / (img%width*img%height)
            b = b / (img%width*img%height)

            AverageImageColor = RGB(int(r), int(g), int(b))

        end function AverageImageColor
end module quad