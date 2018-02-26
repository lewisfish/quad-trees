Module binarytree

    implicit none

    type :: tree
        type(tree), pointer :: left, right
        integer :: data  
    end type tree

    contains

        recursive subroutine add_node(ptr, new_node)

            implicit none

            type(tree), pointer :: ptr, new_node

            if(.not. associated(ptr))then
                allocate(ptr)
                ptr => new_node
            elseif( new_node%data < ptr%data)then
                call add_node(ptr%left, new_node)
            else
                call add_node(ptr%right, new_node)
            end if

        end subroutine add_node


        recursive subroutine write_node(ptr)

            implicit none

            type(tree), pointer :: ptr

            if(associated(ptr%left))then
                call write_node(ptr%left)
            end if

            print*,ptr%data

            if(associated(ptr%right))then
                call write_node(ptr%right)
            end if

        end subroutine write_node
end Module binarytree
program main
    
    use binarytree

    implicit none

    type(tree), pointer :: root
    type(tree), pointer :: tmp

    integer :: i, pfds

    nullify(root, tmp)

    do i = 1, 4
        allocate(tmp)
        nullify(tmp%left, tmp%right)
        read(*,*)pfds
        tmp%data = pfds
        call add_node(root, tmp)
    end do

    call write_node(root)
end program main
