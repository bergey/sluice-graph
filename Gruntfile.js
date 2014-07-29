module.exports = function(grunt) {
    grunt.initConfig({
        requirejs: {
            compile: {
                options: {
                    mainConfigFile: 'require-deploy.js',
                    baseUrl: "js",
                    name: "main",
                    include: [''],
                    out: 'dist/js/config.js'
                }
            }
        },
        bower: {
            target: {
                rjsConfig: 'js/config.js'
            }
        },

        copy: {
            main: {
                files: [
                    {expand: true, src: ['css/*'], dest: 'dist/', filter: 'isFile'},
                    {expand: true, src: ['*html'], dest: 'dist/', filter: 'isFile'},
                    {expand: true, src: ['*csv'], dest: 'dist/', filter: 'isFile'}
                ]}},
        
        'gh-pages': {
            options: {
                base: 'dist'
            },
            src: ['**']
        }
    });

    grunt.loadNpmTasks('grunt-bower-requirejs');
    grunt.loadNpmTasks('grunt-contrib-requirejs');
    grunt.loadNpmTasks('grunt-gh-pages');
    grunt.loadNpmTasks('grunt-contrib-copy');
    
    grunt.registerTask('default', ['bower']);
    grunt.registerTask('dist', ['requirejs', 'copy', ]);
    grunt.registerTask('deploy', ['dist']);
};
