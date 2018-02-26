program main

    use quad,  only : quadtree, display, delete_tree
    use image, only : RGBImage, write_ppm, init_image, alloc_image, RGB, open_image, dealloc_image
    use utils, only : str

    implicit none
    
    type(RGBImage)                :: img, imageout ! images
    type(quadtree)                :: q             ! quadtree
    type(RGB),        allocatable :: cout(:, :)    ! colour array for stupid workaround...
    character(len=:), allocatable :: filename      ! user inputted filename
    integer                       :: io, inte, i   
    character(len=256)            :: arg           ! char for cmd line argments
    logical                       :: border        ! bool for displaying border around nodes or not


    io = command_argument_count()
    inte = 0
    filename = ''

    if(io == 0)then
        print*,'Usage: ./quadimage [file] [options]'
        print*,' '
        print*,'Options: first option is for displaying the border around each node: 1 for true, anything else for false'
        stop
    end if

    do i = 1, io !get cmd line arguments

        call get_command_argument(i, arg)

        if(len_trim(arg) == 0)then
            stop 'No input file'
        end if
        if(i == 1)then
            filename = trim(arg)    
        else if(i == 2)then
            inte = iachar(trim(arg)) - 48 ! change char to int
        end if
    end do

    !set border flag
    border = .false.
    if(inte == 1)border = .true.

    !setup images
    call init_image(img)
    call init_image(imageout)

    call open_image(img, filename(:len(filename)-4), filename(len(filename)-3:))
    call alloc_image(imageout, img%width, img%height)    
    allocate(cout(img%width, img%height))


    !generate quadtree
    allocate(q%children(4))

    print*,'Enter error value:'
    read(*,*)q%errorval

    !create quadtree
    call q%create(img)
    call dealloc_image(img)

    !set outimage to quadtree image
    call display(q, cout, border)

    imageout%colour = cout

    !write out quadtree image
    call write_ppm("out"//filename(:len(filename)-4)//".ppm", imageout, 'P6')
    call execute_command_line("display out"//filename(:len(filename)-4)//".ppm",wait=.false.)

    call sleep(1)
    border = .not. border

    call display(q, cout, border)
    imageout%colour = cout
    ! write out quadtree image with border
    call write_ppm("out"//filename(:len(filename)-4)//"-border.ppm", imageout, 'P6')

    call execute_command_line("display out"//filename(:len(filename)-4)//"-border.ppm",wait=.false.)
    call execute_command_line("display "//filename, wait=.false.)

    !free memory (but not do it properly)
    call delete_tree(q)
    if(associated(q%children))deallocate(q%children)
end program main