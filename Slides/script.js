// Initialize the map
var map = L.map('map').setView([45, 0], 7);

// Add a tile layer to the map, using Esri World Imagery tiles
L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
    attribution: 'Tiles &copy; Esri &mdash; Source: Esri, DeLorme, NAVTEQ'
}).addTo(map);

// Array of locations to fly to, with multiple points for each location
var locations = [
    {
        coords: [47.825601, -3.621055],
        zoom: 9.5,
        points: [
            {coords: [47.697191, -3.190758], message: "Estuaire de la Ria d'Etel"},
            {coords: [47.823039, -3.657152], message: "Estuaire du Belon"},
            {coords: [47.838536, -3.748381], message: "Estuaire de l'Aven"},
            {coords: [47.870931, -4.175167], message: "Estuaire de Pont L'Abbe"}
        ]
    },
    {
        coords: [47.697191, -3.190758],
        zoom: 15,
        points: [
            {coords: [47.697191, -3.190758], message: "Estuaire de la Ria d'Etel"}
        ]
    },
    {
        coords: [47.823039, -3.657152],
        zoom: 14,
        points: [
            {coords: [47.823039, -3.657152], message: "Estuaire du Belon"}
        ]
    },
    {
        coords: [47.838536, -3.748381],
        zoom: 15,
        points: [
            {coords: [47.838536, -3.748381], message: "Estuaire de l'Aven"}
        ]
    },
    {
        coords: [47.870931, -4.175167],
        zoom: 15,
        points: [
            {coords: [47.870931, -4.175167], message: "Estuaire de Pont L'Abbe"}
        ]
    },
    {
        coords: [43.468744, -5.5],
        zoom: 7,
        points: [
            {coords: [43.440166, -3.787469], message: "Estuaire de Santander"},
            {coords: [43.527849, -5.386785], message: "???"},
            {coords: [42.261831, -8.707599], message: "Estuaire de Vigo"}
        ]
    },
    {
        coords: [43.440166, -3.787469],
        zoom: 13,
        points: [
            {coords: [43.440166, -3.787469], message: "Estuaire de Santander"}
        ]
    },
    {
        coords: [43.427327, -3.823126],
        zoom: 15,
        points: [
        ]
    },
    {
        coords: [43.527849, -5.386785],
        zoom: 10,
        points: [
            {coords: [43.527849, -5.386785], message: "???"}
        ]
    },
    {
        coords: [42.261831, -8.707599],
        zoom: 11,
        points: [
            {coords: [42.261831, -8.707599], message: "Estuaire de Vigo"}
        ]
    }
];

// Variable to store all markers
var allMarkers = [];

// Add markers for each point in all locations and store them in allMarkers array
locations.forEach(function(location) {
    location.points.forEach(function(point) {
        var marker = L.marker(point.coords).addTo(map);
        marker.bindPopup(point.message, {autoClose: false}).openPopup();
        allMarkers.push(marker);
    });
});

// Listen for the fragment shown event to animate map
Reveal.addEventListener('fragmentshown', function(event) {
    var fragmentIndex = event.fragment.getAttribute('data-fragment-index');
    var location = locations[fragmentIndex];

    // Fly to the main coordinates of the location
    map.flyTo(location.coords, location.zoom);
});
