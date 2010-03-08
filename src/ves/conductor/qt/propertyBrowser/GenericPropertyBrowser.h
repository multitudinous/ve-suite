#ifndef GENERICPROPERTYBROWSER_H
#define GENERICPROPERTYBROWSER_H

#define QT_NO_KEYWORDS

#include "qttreepropertybrowser.h"
#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>

namespace ves
{
namespace conductor
{

class GenericPropertyBrowser : public QtTreePropertyBrowser
{
Q_OBJECT
public:
    explicit GenericPropertyBrowser(QWidget *parent = 0);

    void setPropertyBrowser( PropertyBrowser* browser );
    void RefreshContents();

Q_SIGNALS:

public Q_SLOTS:

private:
    PropertyBrowser* mBrowser;
    QtDoubleSpinBoxFactory* mDoubleSpinBoxFactory;
    QtSpinBoxFactory* mSpinBoxFactory;
    QtCheckBoxFactory* mCheckBoxFactory;
    QtLineEditFactory* mLineEditFactory;
    QtEnumEditorFactory* mComboBoxFactory;
    QtSliderFactory* mSliderFactory;
};

} // namespace conductor
} // namespace ves

#endif // GENERICPROPERTYBROWSER_H
