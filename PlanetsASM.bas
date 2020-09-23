Attribute VB_Name = "Module3"
'Module3: PlanetsASM.bas by Robert Rayment

Option Base 1
DefLng A-W
DefSng X-Z

'-----------------------------------------------------------

' For calling machine code
Public Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" _
(ByVal lpMCode As Long, _
ByVal Long1 As Long, ByVal Long2 As Long, _
ByVal Long3 As Long, ByVal Long4 As Long) As Long

Public WrapRotateMC() As Byte  ' Array to hold machine code
Public ptMC, ptrStruc          ' Ptrs to Machine Code & Structure

' MCode Structure
Public Type MCodeStruc
   PICW As Long
   PICH As Long
   ixsc As Long
   ixdc As Long
   zLsDpi As Single
   Ptrixdent As Long
   Ptrzrdsq As Long
   PtrPalBGR As Long
End Type
Public MCODE As MCodeStruc
'-------------------------------------


Public Sub ASM_Rotate()  ' Called from Wrap_Rotate

   res = CallWindowProc(ptMC, ptrStruc, 0&, 0&, 0&)

End Sub

Public Sub Loadmcode(InFile$, MCCode() As Byte)
'Load machine code into InCode() byte array
On Error GoTo InFileErr
If Dir$(InFile$) = "" Then
   MsgBox (InFile$ & " missing")
   DoEvents
   Unload Form1
   End
End If
Open InFile$ For Binary As #1
MCSize& = LOF(1)
If MCSize& = 0 Then
InFileErr:
   MsgBox (InFile$ & " missing")
   DoEvents
   Unload Form1
   End
End If
ReDim MCCode(MCSize&)
Get #1, , MCCode
Close #1
On Error GoTo 0
End Sub

