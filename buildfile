Buildr.settings.build['scala.version'] = '2.8.1'
ENV['JAVA_OPTS'] = (ENV['JAVA_OPTS'] || '') + ' -Xmx1024m'

require 'buildr/scala'

define 'extreme-cleverness' do
  project.group = 'com.codecommit'
  project.version = '1.0'

  package
end
