-record(attachment, {id, 
		     name,
                     type,
                     file,
                     thumb,
                     owner,
                     create_data}).

-record(uploads, {key, counter}).