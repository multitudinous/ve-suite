#ifndef VISUALIZATION_H
#define VISUALIZATION_H

#define QT_NO_KEYWORDS

#include <QtGui/QDialog>

#include <ves/conductor/qt/ContourFeatureMaker.h>

// Forward declarations
namespace ves
{
namespace xplorer
{
namespace data
{
class PropertySet;
} // namespace data
} // namespace xplorer

namespace conductor
{
class PropertyBrowser;



namespace Ui {
    class Visualization;
}

class Visualization : public QDialog {
    Q_OBJECT
public:
    Visualization(QWidget *parent = 0);
    ~Visualization();

protected:
    void changeEvent(QEvent *e);

protected Q_SLOTS:
    // For info on Automatic connection of signals and slots, see
    // http://doc.trolltech.com/4.6/designer-using-a-ui-file.html#automatic-connections
    void on_WritePropertiesButton_clicked(); // Automatic connection
    void on_RefreshPropertiesButton_clicked(); // Automatic connection
    void on_NewFeatureButton_clicked(); // Automatic connection
    void on_DeleteFeatureButton_clicked(); // Automatic connection


private:
    Ui::Visualization *ui;
    PropertyBrowser* mFeatureBrowser;
    std::string mDbName;

    xplorer::data::PropertySet* mTempSet;
    ContourFeatureMaker mContourFeatureMaker;
};

} // namespace conductor
} // namespace ves

#endif // VISUALIZATION_H
