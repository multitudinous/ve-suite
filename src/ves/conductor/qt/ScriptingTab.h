#pragma once
#include <QtGui/QWidget>

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
    
private:
    Ui::ScriptingTab *ui;
};

}} // ves::conductor
