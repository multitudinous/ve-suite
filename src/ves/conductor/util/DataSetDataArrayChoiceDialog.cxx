#include <ves/conductor/util/DataSetDataArrayChoiceDialog.h>

#include <iostream>

////////////////////////////////////////////////////////////////////////////////
DataSetDataArrayChoiceDialog::DataSetDataArrayChoiceDialog( wxWindow* parent )
:
DataArrayChoiceDialog( parent )
{
    CentreOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void DataSetDataArrayChoiceDialog::SetDataArrays( std::vector< std::string > activeArrays )
{
    m_activeArrays = activeArrays;
    //Add new strings to dialog
    wxArrayString stingList;
    for( size_t i = 0; i < m_activeArrays.size(); ++i )
    {
        stingList.Add( wxString( m_activeArrays.at( i ).c_str(), wxConvUTF8 ) );
    }
    m_checkList1->InsertItems( stingList, 0 );
    Refresh( true );
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::string > DataSetDataArrayChoiceDialog::GetUserActiveArrays()
{   
    unsigned int numSelections = m_checkList1->GetCount();
    for( size_t i = 0; i < numSelections; ++i )
    {
        if( m_checkList1->IsChecked( i ) )
        {
            m_usersDataArrays.push_back( 
                static_cast< const char* >( 
                wxConvCurrent->cWX2MB( m_checkList1->GetString( i ).c_str() ) ) );
            //std::cout << m_usersDataArrays.back() << std::endl;
        }
    }
    return m_usersDataArrays;
}
////////////////////////////////////////////////////////////////////////////////
