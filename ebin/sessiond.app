{application,sessiond,
             [{description,"A session life server"},
              {id,"sessiond"},
              {vsn,"0.0.1"},
              {modules,[sessiond]},
              {registered,[sessiond]},
              {applications,[kernel,stdlib,sasl]},
              {mod,{sessiond,[]}},
              {env,[{}]}.
