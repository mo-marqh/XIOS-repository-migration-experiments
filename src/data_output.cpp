#include "data_output.hpp"

#include "attribute_template_impl.hpp"
#include "group_template_impl.hpp"
#include "context.hpp"

namespace xmlioserver
{
   namespace io
   {
      /// ////////////////////// Définitions ////////////////////// ///

      CDataOutput::~CDataOutput(void)
      { /* Ne rien faire de plus */ }

      //----------------------------------------------------------------

      void CDataOutput::writeGrid
         (const boost::shared_ptr<tree::CGrid> grid)
      {
         if (grid->domain_ref.isEmpty())
            ERROR("CONetCDF4Adv::writeGrid(grid)",
                   << " domain is not defined !");

         if (grid->axis_ref.isEmpty())
         {
            this->writeGrid
            (CObjectFactory::GetObject<tree::CDomain>(grid->domain_ref.getValue()));
         }
         else
         {
            this->writeGrid
            (CObjectFactory::GetObject<tree::CDomain>(grid->domain_ref.getValue()),
             CObjectFactory::GetObject<tree::CAxis>(grid->axis_ref.getValue()));
         }
      }

      //----------------------------------------------------------------

      void CDataOutput::writeFile
         (const boost::shared_ptr<tree::CFile>  file)
      {
         this->writeFile_(file);
      }
 
      void CDataOutput::syncFile(void)
      {
         this->syncFile_();
      }

      void CDataOutput::closeFile(void)
      {
         this->closeFile_();
      }

      //----------------------------------------------------------------

      void CDataOutput::writeGrid
         (const boost::shared_ptr<tree::CDomain> domain,
          const boost::shared_ptr<tree::CAxis> axis)
      {
         this->writeDomain_(domain);
         this->writeAxis_(axis);
      }

      //----------------------------------------------------------------

      void CDataOutput::writeGrid
         (const boost::shared_ptr<tree::CDomain> domain)
      {
         this->writeDomain_(domain);
      }

      //----------------------------------------------------------------

      void CDataOutput::writeField
         (const boost::shared_ptr<tree::CField> field)
      {
         boost::shared_ptr<tree::CContext> context =
         CObjectFactory::GetObject<tree::CContext>(CObjectFactory::GetCurrentContextId());
         boost::shared_ptr<date::CCalendar> calendar = context->getCalendar();
         
         this->writeField_(field);
         this->writeTimeAxis_(field, calendar);
      }

      //----------------------------------------------------------------

      void CDataOutput::writeFieldGrid
         (const boost::shared_ptr<tree::CField> field)
      {
         this->writeGrid(field->getRelGrid());
      }
      
      //----------------------------------------------------------------
      
      void CDataOutput::writeFieldData(const boost::shared_ptr<tree::CField> field)
      {
         boost::shared_ptr<CGrid> grid =
            CObjectFactory::GetObject<CGrid>(field->grid_ref.getValue());
         boost::shared_ptr<CDomain> domain =
            CObjectFactory::GetObject<CDomain>(grid->domain_ref.getValue());
            
//         if (domain->isEmpty()) return;
         this->writeFieldData_(field);
      }
      
      ///----------------------------------------------------------------

   } // namespace io
} // namespace xmlioserver
