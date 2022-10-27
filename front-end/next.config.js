
/** @type {import('next').NextConfig} */
const nextConfig = {
  
  output: 'standalone',
  reactStrictMode: true,
  webpack: (config) => {
    config.experiments = { 
      asyncWebAssembly: true,
      topLevelAwait: true,
      layers: true
    }
    return config
  }
}

module.exports = nextConfig