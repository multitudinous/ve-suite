#pragma once
#define QT_NO_KEYWORDS
#include <QtGui/QWidget>
#include <QtGui/QFileDialog>

#include <map>
#include <vector>
#include <string>

#ifndef Q_MOC_RUN
#include <vpr/Thread/Thread.h>
#include <switchwire/ScopedConnectionList.h>
#include <switchwire/Event.h>
#endif

#include <propertystore/PropertySetPtr.h>

namespace Ui {
class ScriptingTab;
}

namespace ves
{
namespace conductor
{
class ScriptingTab : public QWidget
{
    Q_OBJECT
    
public:
    explicit ScriptingTab(QWidget *parent = 0);
    ~ScriptingTab();

protected Q_SLOTS:
    void on_m_openButton_clicked();
    void on_m_runButton_clicked();
    void onFileOpenSelected( const QStringList& files );
    void MultiOpenSelected( const QStringList& files );
    void multi_load_clicked();
    void multi_run_clicked();
    void DelayedSetButtonText( QPushButton* button, const QString& text );

Q_SIGNALS:
    void SetButtonTextSignal( QPushButton* button, const QString& text );

protected:
    void runScript( const std::string& scriptText );
    void AssociateScript( const std::string& scriptPath, int buttonIndex );
    
private:
    Ui::ScriptingTab *ui;
    /// Key is the Run button, Value is the path to the script.
    std::map< QObject*, QString > m_scriptMap;
    /// Ordered list of the load buttons on multi-page for use in auto-loading
    /// scripts onto the buttons
    std::vector< QObject* > m_loadButtonVector;
    /// Key is Load button, Value is Run button
    std::map< QObject*, QObject* > m_buttonMap;
    /// The load button on the multi tab that was pushed last
    QObject* m_currentLoadButton;
    /// Pointer to file dialog
    QFileDialog* m_fileDialog;
    /// Stores thread pointer so we can clean up at the end
    std::vector< vpr::Thread* > m_threads;

    vpr::Thread* m_partManipThread;

    void StartPartManipulatorScript();
    void StopPartManipulatorScript();
    void HandleCADSelection( bool const& );

    switchwire::ScopedConnectionList m_connections;
    switchwire::ScopedConnectionList m_scenegraphChangedConnection;
    switchwire::ScopedConnectionList m_cleanupConnection;

    typedef switchwire::Event< void( bool ) > VoidBoolSignalType;
    VoidBoolSignalType m_destroySignal;

    void ScheduleApplyPartManipulatorPropertySets();
    void ApplyPartManipulatorPropertySets();
    void CleanUpLoadingPartManipPropertySets();

    std::vector< propertystore::PropertySetPtr > m_loadingPartManipPropertySets;
};

}} // ves::conductor
