Module SLL 
   type node
      integer:: data
      type(node),pointer::next   
   end type node 

   type list 
      type(node),pointer::head=>null() 
      type(node),pointer::tail=>null() 
      contains 
         procedure::append 
         procedure::display 
         procedure::getLen 
         procedure::insert  
         procedure::delete 
   end type list  

   contains 
function new(data)result(newNode)
   implicit none 
   integer::data 
   type(node),pointer::newNode 

   allocate(newNode)
   newNode%data=data 
   newNode%next=>null() 
end function new 

subroutine append(self,data)
   implicit none 
   class(list)::self 
   integer:: data 
   type(node),pointer::newNode 

   newNode=>new(data) 
   if(.not.associated(self%head))then 
      self%head=>newNode 
      self%tail=>newNode 
      newNode=>null() 
   else
      self%tail%next=>newNode 
      self%tail=>newNode
      newNode=>null() 
   endif 
end subroutine 

subroutine insert(self,pos,data)
   implicit none 
   class(list)::self 
   integer::pos 
   integer:: data 
   type(node),pointer::newNode,iterator,right 
   integer::front,last,ipos  

   front=1
   last=self%getLen()+1 
   
   newNode=>new(data) 
   if(pos==front)then 
      newNode%next=>self%head 
      self%head=>newNode 
   elseif (pos>=last .or. pos==-1)then
      self%tail%next=>newNode 
      self%tail=>newNode
      newNode=>null() 
   else 
      iterator=>self%head 
      ipos=0
      do while(associated(iterator))
         ipos=ipos+1 
         if(ipos==pos)then 
            right=>iterator%next  
            newNode%next=>right 
            iterator%next=>newNode 
         else  
            iterator=>iterator%next
         endif  
      enddo 
   endif 

end subroutine 

function  getLen(self)result(len)
   implicit none 
   class(list)::self 
   integer:: len 
   type(node),pointer::iterator 

   iterator=>self%head 
   
   len=0
   do while(associated(iterator))
      len=len+1 
      iterator=>iterator%next
   enddo 
end function getLen 

subroutine display(self)
   implicit none 
   class(list)::self 
   integer:: data 
   type(node),pointer::newNode 

   newNode=>self%head 
   
   do while(associated(newNode))
      print*,(newNode%data)
      newNode=>newNode%next
   enddo 
end subroutine display 

subroutine delete(self,pos)
   implicit none 
   class(list)::self 
   integer::pos 
   integer:: data 
   type(node),pointer::temp,current,previous    
   integer::front,last,ipos  

   front=1
   
   if(pos==front)then
      temp=>self%head 
      self%head=>self%head%next 
      deallocate(temp)

   elseif (pos==-1)then
      current=>self%head 
      do while(associated(current%next))
         previous=>current 
         current=>current%next 
      enddo 
      self%tail=>previous 
      self%tail%next=>null() 
      deallocate(current)

   else 

      ipos=0
      current=>self%head
      do ipos=1,pos-1  
         previous=>current 
         current=>current%next 
      enddo 
      previous%next=>current%next 
      deallocate(current)

   endif 

end subroutine 


End Module SLL 

program main 
   use SLL 
   type(list)::mylist 

   call mylist%append(5)
   call mylist%append(6)
   call mylist%append(7)
   call myList%insert(1,4)
   call myList%insert(1,3)
   call myList%insert(7,8)
   call myList%insert(2,-1)
   call myList%delete(1)
   call myList%delete(1)
   call myList%delete(-1)
   call myList%delete(3)
   call mylist%display() 
   

   print*,(myList%getLen())

end program main 
