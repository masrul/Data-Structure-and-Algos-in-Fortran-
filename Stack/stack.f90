Module Stack_Module  
   type node
      integer:: data
      type(node),pointer::next   
   end type node 

   type stack 
      integer::size=0 
      type(node),pointer::head=>null() 
      type(node),pointer::tail=>null() 
      contains 
         procedure::push  
         procedure::pop
         procedure::top  
         procedure::isEmpty  
         procedure::kill 
         procedure::display 
   end type stack 

contains 
   function new(data)result(new_node)
      implicit none 
      integer::data 
      type(node),pointer::new_node 

      allocate(new_node)
      new_node%data=data 
      new_node%next=>null() 
   end function new 

   subroutine push(self,data)
      implicit none 
      class(stack)::self 
      integer:: data 
      type(node),pointer::new_node 

      new_node=>new(data) 
      if(.not.associated(self%head))then 
         self%head=>new_node 
         self%tail=>new_node 
         new_node=>null() 
      else
         new_node%next=>self%head 
         self%head=>new_node 
      endif 
      self%size=self%size+1 
   end subroutine 

   function isEmpty(self)result(bool)
      implicit none 
      class(stack)::self 
      logical::bool 

      bool=.not.associated(self%head)
   end function 

   subroutine display(self)
      implicit none 
      class(stack)::self 
      integer:: data 
      type(node),pointer::new_node 

      new_node=>self%head 
      
      write(*,'(A)',advance='no')'Stack: '
      do while(associated(new_node))
         write(*,'(I3)',advance='no')(new_node%data)
         new_node=>new_node%next
      enddo 
      write(*,'(I3)',advance='yes')
   end subroutine display 

   subroutine pop(self)
      implicit none 
      class(stack)::self 
      integer:: data 
      type(node),pointer::temp 

      data=self%head%data 
      temp=>self%head 
      self%head=>self%head%next 
      deallocate(temp)
      self%size=self%size-1 
   end subroutine  

   function top(self)result(data)
      implicit none 
      class(stack)::self 
      integer:: data 
      type(node),pointer::temp 

      data=self%head%data 
   end function  

   subroutine kill(self)
      implicit none 
      class(stack)::self 
      integer::pos 
      integer:: data 
      type(node),pointer::temp  

     
      do while(associated(self%head))
         temp=>self%head 
         self%head=>self%head%next 
         deallocate(temp)
      enddo 
   end subroutine 

End Module Stack_Module  

program main 
   use Stack_Module  
   type(stack)::mystack 

   call mystack%push(5)
   call mystack%push(6)
   call mystack%display()
   print*,mystack%top()
   call mystack%pop() 
   print*,mystack%top()
   print*,mystack%size,mystack%isEmpty() 
   call mystack%pop() 
   print*,mystack%size,mystack%isEmpty() 

end program main 
