#pragma once

#include <ves/conductor/qt/MainWindow.h>
#include <ves/VEConfig.h>

// --- VR Juggler includes --- //
#include <vpr/Util/Singleton.h>

// --- Boost includes --- //
#include <boost/noncopyable.hpp>

namespace ves
{
namespace conductor
{
/*!\file UITabs.h
 * \class ves::conductor::UITabs
 * \namespace ves::conductor
 *
 */

/// This class exists to provide a singleton interface into the add/remove tab methods
/// of ves::conductor::MainWindow. A singleton implementation was chosen over
/// a group of signals/slots because the functions wrapped here are more easily
/// conceived of as procedural calls rather than reactions to an event. This
/// class was set up as a separate singleton (rather than making MainWindow
/// itself a singleton) because MainWindow's lifetime is partly managed by Qt,
/// and there would be the danger of double deletion of the class.
class VE_CONDUCTOR_QTUI_EXPORTS UITabs: public boost::noncopyable
{
public:
    /// Sets the child widget for this singleton. All calls to other public
    /// methods will be passed directly to this child.
    void SetChild( ves::conductor::MainWindow* child );

    /// Adds @c widget to tabs and gives tab the label @c tabLabel.
    int AddTab( QWidget* widget, const std::string& tabLabel );

    /// Remove tab containing @c widget. Does not delete the widget.
    void RemoveTab( QWidget* widget );

    /// Remove tab with label @c tabLabel. Does not delete the associated widget.
    void RemoveTab( const std::string& tabLabel );

    /// Remove all existing tabs. Does not delete the underlying widgets.
    void RemoveAllTabs();

    /// Activate tab containing @c widget
    void ActivateTab( QWidget* widget );

    /// Activate tab with label @c tabLabel
    void ActivateTab( const std::string& tabLabel );

    /// Activate tab at index @c index
    void ActivateTab( int index );

private:
    UITabs( );
    virtual ~UITabs();
    /// Singleton declarations
    vprSingletonHeader( UITabs );

    ves::conductor::MainWindow* m_child;
};

}
}
