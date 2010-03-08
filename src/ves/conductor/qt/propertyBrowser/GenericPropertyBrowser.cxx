#define QT_NO_KEYWORDS

#include "qtpropertymanager.h"
#include "qteditorfactory.h"

#include <ves/conductor/qt/propertyBrowser/GenericPropertyBrowser.h>

using namespace ves::conductor;

GenericPropertyBrowser::GenericPropertyBrowser(QWidget *parent) :
    QtTreePropertyBrowser(parent)
{
    // Create the default editor types we want to use
    mDoubleSpinBoxFactory = new QtDoubleSpinBoxFactory(this);
    mSpinBoxFactory = new QtSpinBoxFactory(this);
    mCheckBoxFactory = new QtCheckBoxFactory(this);
    mLineEditFactory = new QtLineEditFactory(this);
    mComboBoxFactory = new QtEnumEditorFactory(this);
    mSliderFactory = new QtSliderFactory(this);
}

void GenericPropertyBrowser::setPropertyBrowser( PropertyBrowser* browser )
{
    mBrowser = browser;

    // Associate editor types with property types
    this->setFactoryForManager( browser->GetBoolManager(), mCheckBoxFactory );
    this->setFactoryForManager( browser->GetIntManager(), mSliderFactory );
    this->setFactoryForManager( browser->GetDoubleManager(), mDoubleSpinBoxFactory );
    this->setFactoryForManager( browser->GetStringManager(), mLineEditFactory );
    this->setFactoryForManager( browser->GetEnumManager(), mComboBoxFactory );
}

void GenericPropertyBrowser::RefreshContents()
{
    this->clear();

    this->setResizeMode(QtTreePropertyBrowser::ResizeToContents);

    PropertyBrowser::ItemVector* items = mBrowser->GetItems();
    PropertyBrowser::ItemVector::iterator iterator;
    PropertyBrowser::ItemVector::iterator end = items->end();
    for( iterator = items->begin(); iterator != end; iterator++)
    {
        this->addProperty( (*iterator) );
    }
    this->setResizeMode(QtTreePropertyBrowser::Interactive);
}
