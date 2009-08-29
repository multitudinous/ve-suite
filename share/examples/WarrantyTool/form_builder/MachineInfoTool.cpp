#include "MachineInfoTool.h"

////////////////////////////////////////////////////////////////////////////////
MachineInfoTool::MachineInfoTool( wxWindow* parent )
:
MachineInfoDlg( parent )
{

}
////////////////////////////////////////////////////////////////////////////////
void MachineInfoTool::OnDataLoad( wxFileDirPickerEvent& event )
{
	// TODO: Implement OnDataLoad
    //Parse the csv file
    //tell ves to load
    //Populate all of the choice dialog boxes with the appropriate data
}
////////////////////////////////////////////////////////////////////////////////
void MachineInfoTool::OnVariableAndLogicalChoice( wxCommandEvent& event )
{
	// TODO: Implement OnVariableAndLogicalChoice
    //When the user selects a logical operator
    //figure out which one it is
    //Figure out if we need to activate any of the sub choice boxes
    //The update the text display box
}
////////////////////////////////////////////////////////////////////////////////
void MachineInfoTool::OnCreateInputText( wxCommandEvent& event )
{
	// TODO: Implement OnCreateInputText
    //Get the text from the user and update the query text display
}
////////////////////////////////////////////////////////////////////////////////
void MachineInfoTool::OnTextQueryEnter( wxCommandEvent& event )
{
	// TODO: Implement OnTextQueryEnter
    //When the user types in their on text entry submit the query
}
////////////////////////////////////////////////////////////////////////////////
void MachineInfoTool::OnPartSelection( wxCommandEvent& event )
{
	// TODO: Implement OnPartSelection
    //When the user selects a part number submit it and update the associated 
    //text entry box
}
////////////////////////////////////////////////////////////////////////////////
void MachineInfoTool::OnPartNumberEntry( wxCommandEvent& event )
{
	// TODO: Implement OnPartNumberEntry
    //When a user types in a part number to find submit it and go find it
}
////////////////////////////////////////////////////////////////////////////////
void MachineInfoTool::OnQueryApply( wxCommandEvent& event )
{
	// TODO: Implement OnQueryApply
    //Submit the command currently in the query text box
    
}
////////////////////////////////////////////////////////////////////////////////
void MachineInfoTool::OnDialogCancel( wxCommandEvent& event )
{
	// TODO: Implement OnDialogCancel
    
    //Do not do anything and close the dialog
}
////////////////////////////////////////////////////////////////////////////////
void MachineInfoTool::OnQueryOK( wxCommandEvent& event )
{
	// TODO: Implement OnQueryOK
    //Submit the command currently in the query text box and close the dialog
}
////////////////////////////////////////////////////////////////////////////////
