module.exports = {
	globDirectory: 'dist/',
	globPatterns: [
		'**/*.{html,js,css,png,svg,jpg,gif,json,woff,woff2,eot,ico,webmanifest,map,wav}'
	],
	ignoreURLParametersMatching: [
		/^utm_/,
		/^fbclid$/
	],
	swDest: 'dist/sw.js',
	maximumFileSizeToCacheInBytes: 3145728
};
