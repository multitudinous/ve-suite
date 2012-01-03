#include <ves/conductor/qt/UITabs.h>

namespace ves
{
namespace conductor
{

vprSingletonImp( UITabs );

UITabs::UITabs():
    m_child( 0 )
{
}

UITabs::~UITabs()
{
}

void UITabs::SetChild( ves::conductor::MainWindow* child )
{
    m_child = child;
}

int UITabs::AddTab( QWidget* widget, const std::string& tabLabel, bool deleteOnClose )
{
    return m_child->AddTab( widget, tabLabel, deleteOnClose );
}

void UITabs::RemoveTab( QWidget* widget )
{
    m_child->RemoveTab( widget );
}

void UITabs::RemoveTab( const std::string& tabLabel )
{
    m_child->RemoveTab( tabLabel );
}

void UITabs::RemoveAllTabs()
{
    m_child->RemoveAllTabs();
}

void UITabs::ActivateTab( QWidget* widget )
{
    m_child->ActivateTab( widget );
}

void UITabs::ActivateTab( const std::string& tabLabel )
{
    m_child->ActivateTab( tabLabel );
}

void UITabs::ActivateTab( int index )
{
    m_child->ActivateTab( index );
}

}
}
